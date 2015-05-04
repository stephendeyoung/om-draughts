
(ns draughts.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [draughts.data :as data]))

(enable-console-print!)

(defn create-piece [data component]
  (reify
    om/IRenderState
    (render-state [_ state]
      (dom/span #js {:className (str "piece " (:type data))
                     :draggable true
                     :onDragStart (fn [e]
                                    (.. e -nativeEvent -dataTransfer (setData "text/plain" nil))
                                    (om/set-state! (:parent-component state) :dragged @data))}))))

(defn valid-direction [[cell-x cell-y] [piece-x piece-y] direction jumps]
  (if (and (= (direction piece-y jumps) cell-y)
           (or (= (- piece-x jumps) cell-x) (= (+ piece-x jumps) cell-x)))
    true
    false))

(defn find-index-of-item [cell]
  (reduce
    (fn [x y]
      (+ x (* (- 9 y) 10)))
    cell))

(defn intervening-square-status [parent-component [cell-x cell-y :as cell] dragged]
  (let [data @(om/get-props parent-component)
        dragged-type (:type dragged)
        direction (if (= dragged-type "white")
                    {:forwards inc :backwards dec}
                    {:forwards dec :backwards inc})
        intervening-square1 (get data
                                 (find-index-of-item
                                   (mapv (:backwards direction) cell)))
        intervening-square2 (get data
                                 (find-index-of-item
                                   [((:forwards direction) cell-x)
                                    ((:backwards direction) cell-y)]))]

    (cond
		 (and (:active intervening-square1) (not= (:type intervening-square1) dragged-type)) intervening-square1
     (and (:active intervening-square2) (not= (:type intervening-square2) dragged-type)) intervening-square2
     :else {:active false :position nil :type nil})))

(defn can-piece-move-here [data dragged parent-component]
  (let [direction (if (= (:type dragged) "white")
                    +
                    -)
        cell (:position @data)
        piece (:position dragged)
				intervening-square (intervening-square-status parent-component cell dragged)]
		(cond
		 (= (:active @data) true) {:can-move false :capture false}
		 (valid-direction cell piece direction 1) {:can-move true :capture false}
		 (and (valid-direction cell piece direction 2) (:active intervening-square)) {:can-move true :capture intervening-square}
		 :else {:can-move false :capture false})))

(defn handle-drag-enter [e state data component]
  (.preventDefault e)
  (let [parent-component (:parent-component state)
        dragged (om/get-state parent-component :dragged)]
    (if (:can-move (can-piece-move-here data dragged parent-component))
      (om/set-state! component :valid-move "valid")
      (if (not= dragged @data)
       (om/set-state! component :valid-move "not-valid")))))

(defn board-square [data component]
  (reify
    om/IRenderState
    (render-state [_ state]
      (dom/td #js {:className (:valid-move state)
                   :onDragOver (fn [e] (.preventDefault e))
                   :onDragEnter (fn [e] (handle-drag-enter e state data component))
                   :onDragLeave (fn [_] (om/set-state! component :valid-move nil))
                   :onDrop (fn [e]
                             (.preventDefault e)
                             (let [parent-component (:parent-component state)
                                   dragged (om/get-state parent-component :dragged)
																	 move-data (can-piece-move-here data dragged parent-component)]
                               (if (:can-move move-data)
                                 (put! (:update state) [dragged @data (:capture move-data)]))
                               (om/set-state! component :valid-move nil)))}
              (if (:active data)
                (om/build create-piece data {:init-state
                                             {:parent-component (:parent-component state)}}))))))

(defn build-table [cells state]
  (loop [squares cells
         elements []]
    (if (empty? squares)
      elements
      (let [row (take 10 squares)]
        (recur (drop 10 squares)
               (conj
                 elements
                 (apply dom/tr
                        nil
                        (om/build-all board-square
                                      row
                                      {:init-state state}))))))))


(defn table-body [data component]
  (reify
    om/IInitState
    (init-state [_]
      {:update (chan)
       :parent-component component})
    om/IWillMount
    (will-mount [_]
      (go (loop []
            (let [[old-loc new-loc captured-piece] (<! (om/get-state component :update))]
              (om/transact! data
                            (fn [old-data]
                              (mapv (fn [square]
                                     (cond
                                       (= square new-loc)
                                        {:position (:position square)
                                          :active true
                                          :type (:type old-loc)}
                                       (= square old-loc)
                                        {:position (:position old-loc)
                                         :active false
                                         :type nil}
																			 (= square captured-piece)
																				{:position (:position captured-piece)
                                         :active false
                                         :type nil}
                                       :else square))
                                   old-data)))
              (recur)))))
    om/IRenderState
    (render-state [_ state]
      (apply dom/tbody
             nil
             (build-table data state)))))

(om/root
  table-body
  data/pieces
  {:target (. js/document (getElementById "draughts"))})
