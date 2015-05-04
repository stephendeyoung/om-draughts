
(ns draughts.data)
(enable-console-print!)
(defn create-data [any-active type col-index position]
  {:position (into [col-index] position)
   :active (cond
             (and any-active (and (even? (first position))
                                  (even? col-index))) true
             (and any-active (and (odd? (first position))
                                  (odd? col-index))) true
             :else false)
   :type type})

(defn build-data [start-index iterations any-active type]
  (loop [index start-index
         times iterations
         items []]
    (if (= times 0)
      items
      (recur (dec index)
             (dec times)
             (into [] (concat items
                       (map-indexed
                         (partial create-data any-active type)
                         (repeat 10 [index]))))))))

(def pieces
  (atom (into [] (concat
                   (build-data 9 4 true "black")
                   (build-data 5 2 false nil)
                   (build-data 3 4 true "white")))))
