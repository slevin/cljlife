(ns cljlife.core
  (:import (java.awt Graphics))
  (:use seesaw.core seesaw.color seesaw.graphics)
  (:require [clojure.math.numeric-tower :refer [floor]])
  (:gen-class))


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




;; main
;;  read in starting state
;;  print starting state
;;  while true
;;   readln
;;   set next state
;;   print it

(def main-state (ref nil))

(defn load-starting-state []
  (dosync
   (ref-set main-state
            [[o x o]
             [o x o]
             [o x o]])))

(defn run-ascii-loop []
  (println (state-string @main-state))
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (dosync
     (ref-set main-state (next-state @main-state)))
    (println (state-string @main-state))))


(def gap 2)

(defn color-from-state [state-item]
  (cond (= state-item x) (color 0 0 0)
        (= state-item o) (color 255 255 255)))

(defn state-to-coords [state width height]
  (let [rowcount (count state)
        rowheight (floor (/ height rowcount))]
    (map-indexed (fn [rownumber row]
                   (let [colcount (count row)
                         colwidth (floor (/ width colcount))
                         y (+ (* gap rownumber) (* rowheight rownumber))]
                     (map-indexed (fn [colnumber col]
                                    (let [x (+ (* gap colnumber) (* colwidth colnumber))]
                                      [(color-from-state col) x y colwidth rowheight]))
                                  row)))
                 state)))

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

(defn run-gui []
  (invoke-later
   (-> (frame :title "Life"
              :content (border-panel :hgap 5 :vgap 5 :border 5
                                     :center (canvas :id :canvas :background "#BBBBDD" :paint paint-board)
                                     :south (horizontal-panel :items [(action :name "next phase" :handler handle-gui-state-update)])))
       pack!
       show!)))

(defn -main
  [& args]
  (load-starting-state)
;;  (run-ascii-loop))
  (run-gui))


;; (defn -main
;;   [& args]
;;   (invoke-later
;;    (-> (frame :title "Hello",
;;               :content "Hello, seesaw",
;;               :on-close :exit)
;;        pack!
;;        show!)))
