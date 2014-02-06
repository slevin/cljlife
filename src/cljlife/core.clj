(ns cljlife.core
  (:import (java.awt Graphics) (java.util Date))
  (:use seesaw.core seesaw.color seesaw.graphics seesaw.dev)
  (:require [clojure.math.numeric-tower :refer [floor]])
  (:gen-class))

(use '[seesaw.mouse :only (location)])

(declare next-item live-neighbors neighbors)
(def o \space)
(def x \*)

(defn next-state [current-state]
  (map-indexed (fn [row-number row]
                 (map-indexed (fn [col-number col]
                                (next-item row-number col-number current-state))
                              row))
               current-state))

(defn next-item [row-number col-number current-state]
  (let [current-item (nth (nth current-state row-number) col-number)
        ns (neighbors row-number col-number current-state)
        live-ns (live-neighbors ns)
        live-count (count live-ns)]
    (if (= current-item x)
      (case live-count 
        (0 1) o
        (2 3) x
        \space)
      (case live-count
        (3) x
        o))))

(defn live-neighbors [neighbors]
  (filter (fn [item] (= item \*)) neighbors))

(defn neighbors [row-number col-number current-state]
  [(nth (nth current-state (- row-number 1) []) (- col-number 1) o)
   (nth (nth current-state (- row-number 1) []) col-number o)
   (nth (nth current-state (- row-number 1) []) (+ col-number 1) o)
   (nth (nth current-state row-number []) (- col-number 1) o)
   (nth (nth current-state row-number []) (+ col-number 1) o)
   (nth (nth current-state (+ row-number 1) []) (- col-number 1) o)
   (nth (nth current-state (+ row-number 1) []) col-number o)
   (nth (nth current-state (+ row-number 1) []) (+ col-number 1) o)])

(defn state-string [current-state]
  (clojure.string/join "\n" (map (fn [row] (apply str row)) current-state)))




;; store the current board
(def main-state (ref nil))

;; current status of mouse hover
(def hover-xy (ref nil))


(defn load-starting-state []
  (dosync
   (ref-set main-state
            [[o x o]
             [o x o]
             [o x o]])))


;; adds or removes extra rows and columns, keeping old stuff if possible
(defn update-state-size [state size]
  ;get current size by padding or removing rows
  (let [adjusted-state (cond (<= size (count state)) (take size state)
                             :else (concat state (repeat (- size (count state)) [])))]
    (map (fn [row]
           (cond (<= size (count row)) (take size row)
                 :else (concat row (repeat (- size (count row)) o))))
         adjusted-state)))


(defn run-ascii-loop []
  (println (state-string @main-state))
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (dosync
     (ref-set main-state (next-state @main-state)))
    (println (state-string @main-state))))


(def gap 2)

(defn color-from-state [state-item col row]
  (if (= [col row] @hover-xy)
    (cond (= state-item x) (color 50 50 50)
          (= state-item o) (color 225 225 225))
    (cond (= state-item x) (color 0 0 0)
          (= state-item o) (color 255 255 255))))

(defn state-to-coords [state width height]
  (let [rowcount (count state)
        rowheight (- (/ height rowcount) gap)]
    (map-indexed (fn [rownumber row]
                   (let [colcount (count row)
                         colwidth (- (/ width colcount) gap)
                         y (+ (+ (* gap rownumber) (* rowheight rownumber)) (/ gap 2))]
                     (map-indexed (fn [colnumber col]
                                    (let [x (+ (+ (* gap colnumber) (* colwidth colnumber)) (/ gap 2))]
                                      [(color-from-state col colnumber rownumber) x y colwidth rowheight]))
                                  row)))
                 state)))

(defn coords-to-state-index [coord-x coord-y width height state]
  (let [rowcount (count state)
        rowheight (/ height rowcount)
        rowindex (floor (/ coord-y rowheight))
        row (nth state rowindex)
        colcount (count row)
        colwidth (/ width colcount)
        colindex (floor (/ coord-x colwidth))]
    [colindex rowindex]))


(defn toggled-state [colindex rowindex state]
  (map-indexed (fn [map-row-index row]
                 (if (= map-row-index rowindex)
                   (map-indexed (fn [map-col-index col]
                                  (if (= map-col-index colindex)
                                    (if (= col x)
                                      o x)
                                    col))
                                row)
                   row))
               state))



(defn fn-fillRect [g x y w h]
  (.fillRect g x y w h))

(defn paint-coords [g coords]
  (.setColor g (color 255 255 255))
  (doseq [row coords]
    (doseq [col row]
      (.setColor g (first col))
      ;; apply col values to java call requires
      ;; a fn, which the java interop doesn't supprt directly
      (apply (fn [g x y w h]
               (.fillRect g x y w h)) g (rest col)))))


(defn paint-board [c g]
  (let [w (.getWidth c)
        h (.getHeight c)
        coords (state-to-coords @main-state w h)]
    (paint-coords g coords)))


(defn update-gui []
    (dosync
     (ref-set main-state (next-state @main-state)))
    (paint-board))

(defn handle-gui-state-update [event]
  (dosync
   (ref-set main-state (next-state @main-state)))
  (-> (to-frame event)
      (select [:#canvas])
      repaint!))

(defn handle-field-size-update [event]
  (let [slide (to-widget event)
        window (to-root slide)
        new-size (value slide)]
    ;; events triggered before its fully configured so I ignore those
    ;; until its ready
    (when-not (or (nil? new-size) (nil? window))
      (dosync
       (ref-set main-state (update-state-size @main-state new-size)))
      (-> (to-frame slide)
          (select [:#canvas])
          repaint!))))

(defn slider-widget []
  (slider
   :id :size-slider
   :orientation :horizontal
   :value 3
   :min 3
   :max 20
   :snap-to-ticks? true
   :paint-ticks? true
;;   :text "Field Dimension"
   :listen [:change handle-field-size-update]))

(defn board-clicked [event]
  (let [[x y] (location event)
        canvas (to-widget event)
        w (.getWidth canvas)
        h (.getHeight canvas)
        [col-index row-index] (coords-to-state-index x y w h @main-state)]
    (dosync
     (ref-set main-state (toggled-state col-index row-index @main-state)))
    (repaint! canvas)))

(defn update-hover-to [x y]
  (dosync
   (ref-set hover-xy [x y])))

(defn clear-hover []
  (dosync
   (ref-set hover-xy nil)))

(defn mouse-moved [event]
  (let [[x y] (location event)
        canvas (to-widget event)
        w (.getWidth canvas)
        h (.getHeight canvas)
        [new-col-index new-row-index] (coords-to-state-index x y w h @main-state)]
    (if (nil? @hover-xy) 
      (do
        (update-hover-to new-col-index new-row-index) 
        (repaint! canvas))
      (let [[old-col-index old-row-index] @hover-xy]
        (if-not (and (= old-col-index new-col-index) 
                     (= old-row-index new-row-index))
          (do
            (update-hover-to new-col-index new-row-index)
            (repaint! canvas)))))))
        
(defn mouse-exited [event]
  (let [canvas (to-widget event)]
    (clear-hover)
    (repaint! canvas)))

(defn run-gui []
  (invoke-later
   (-> (frame :title "Life"
              :content (border-panel :hgap 5 :vgap 5 :border 5
                                     :center (canvas :id :canvas :background "#BBBBDD" :paint paint-board :listen [:mouse-released board-clicked :mouse-moved mouse-moved :mouse-exited mouse-exited])
                                     :south (horizontal-panel :items [(action :name "next phase" :handler handle-gui-state-update) (slider-widget)])))
       pack!
       show!)))

(defn -main
  [& args]
  (load-starting-state)
;;  (run-ascii-loop))
  (run-gui))

