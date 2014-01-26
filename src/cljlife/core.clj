(ns cljlife.core
  (:use seesaw.core)
  (:gen-class))

;; (defn -main
;;   [& args]
;;   (invoke-later
;;    (-> (frame :title "Hello",
;;               :content "Hello, seesaw",
;;               :on-close :exit)
;;        pack!
;;        show!)))

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



;; (def main-state (ref nil))

;; (def load-starting-state
;;   (ref-set main-state
;;            [[\space \*]]))
;; (defn -main)
