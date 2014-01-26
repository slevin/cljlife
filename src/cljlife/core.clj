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
    (if (= current-item \*)
      (case live-count 
        (0 1) \space
        (2 3) \*
        \space)
      (case live-count
        (3) \*
        \space))))

(defn live-neighbors [neighbors]
  (filter (fn [item] (= item \*)) neighbors))

(defn neighbors [row-number col-number current-state]
  [(nth (nth current-state (- row-number 1) []) (- col-number 1) \space)
   (nth (nth current-state (- row-number 1) []) col-number \space)
   (nth (nth current-state (- row-number 1) []) (+ col-number 1) \space)
   (nth (nth current-state row-number []) (- col-number 1) \space)
   (nth (nth current-state row-number []) (+ col-number 1) \space)
   (nth (nth current-state (+ row-number 1) []) (- col-number 1) \space)
   (nth (nth current-state (+ row-number 1) []) col-number \space)
   (nth (nth current-state (+ row-number 1) []) (+ col-number 1) \space)])

(defn state-string [current-state]
  (clojure.string/join "\n" (map (fn [row] (apply str row)) current-state)))

