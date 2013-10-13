(ns seldon.draw
  (:require [seldon.core :refer [simple-test-proc]]
            [clojure.core.async :refer [go <!!]]
            [quil.core :refer :all]))

(def avg-radius 20)

(def tile-width 100)
(def tile-height 100)

(defn -radius [pop]
  (/ (:population (:stocks pop)) 5))

(defn pop->circle [pop i tile-x tile-y]
  (let [memome (:memome pop)]
    {:x        (+ (* tile-x tile-width) avg-radius (* avg-radius i))
     :y        (+ (* tile-y tile-height) avg-radius)
     :radius   (-radius pop)
     :fill-col (color (* 255 (:pastorialism memome))
                      (* 255 (:forest-gardening memome))
                      (* 255 (:agriculture memome)))
     :alph     255}))

(defn setup []
  (background 255)
  (smooth)
  (stroke-weight 1)
  (fill-int 150 50)
  (set-state! :grid (atom [])))

(defn draw-circle
  [{:keys [x y radius fill-col alph]}]
  (no-stroke)
  (fill-int fill-col alph)
  (ellipse x y (* 2 radius) (* 2 radius)))

(defn draw-grid [tile-w tile-h]
  (stroke 0 0 0)
  (dotimes [tile-x tile-w]
    (line (* tile-x tile-width) (height)
          (* tile-x tile-width) 0))
  (dotimes [tile-y tile-h]
    (line 0 (* tile-y tile-height)
          (height) (* tile-y tile-height)))
  (no-stroke))

(defn draw [in]
  (when (= 0 (mod (frame-count) 30))
    (let [grid (<!! in)]
      (let [circles
            (flatten (map-indexed
                      (fn [x col]
                        (map-indexed
                         (fn [y tile]
                           (map-indexed
                            (fn [i pop]
                              ;; (println "POP" i pop)
                              (pop->circle pop i x y))
                            (:pops tile)))
                         col))
                      grid))]
        (swap! (state :grid) (constantly [grid circles])))))

  (when-let [[grid circles] @(state :grid)]
    (let [mx (mouse-x) my (mouse-y)
          tile-x (quot mx tile-width)
          tile-y (quot my tile-height)
          i (quot (- mx (* tile-x tile-width)
                     (/ avg-radius 2))
                  avg-radius)
          pop (get (:pops (get-in grid [tile-x tile-y])) i)]
      (background 255)
      (draw-grid (count grid)
                 (count (first grid)))
      (doseq [circle circles]
        (draw-circle circle))
      (when pop
        (let [radius (-radius pop)]
          (stroke 0 0 0)
          (no-fill)
          (ellipse (+ (* tile-x tile-width) avg-radius (* avg-radius i))
                   (+ (* tile-y tile-height) avg-radius)
                   (* 2 radius)
                   (* 2 radius))
          (no-stroke)
          (fill-int (color 0 0 0) 255)
          (text (with-out-str (clojure.pprint/pprint pop))
                (- (width) 300)
                20))))))

(defsketch seldon
  :title "Seldon viewer"
  :setup setup
  :draw (partial draw (simple-test-proc))
  :size [500 300])
