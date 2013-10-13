(ns seldon.draw
  (:require [seldon.core :refer [simple-test-proc]]
            [clojure.core.async :refer [go <!!]]
            [quil.core :refer :all]))

(def avg-radius 30)

(def tile-width 100)
(def tile-height 100)

(defn pop->circle [pop i tile-x tile-y]
  (let [memome (:memome pop)]
    {:x        (+ (* tile-x tile-width) avg-radius (* avg-radius i))
     :y        (+ (* tile-y tile-height) avg-radius)
     :radius   (:population (:stocks pop))
     :fill-col (color (* 255 (:pastorialism memome))
                      (* 255 (:forest-gardening memome))
                      (* 255 (:bow-and-arrow memome)))
     :alph     255}))

(defn setup []
  (background 255)
  (smooth)
  (stroke-weight 1)
  (fill-int 150 50))

(defn draw-circle
  [{:keys [x y radius fill-col alph]}]
  (no-stroke)
  (fill-int fill-col alph)
  (ellipse x y (* 2 radius) (* 2 radius)))

(defn draw [in]
  (let [grid (<!! in)]
    (background 255)
    (doall (map-indexed
            (fn [x col]
              (doall (map-indexed
                      (fn [y tile]
                        (println ":pops tile" (:pops tile))
                        (doall (map-indexed
                                (fn [i pop]
                                  (println i)
                                  (draw-circle (pop->circle pop i x y)))
                                (:pops tile))))
                      col)))
            grid))))

(defsketch seldon
  :title "Seldon viewer"
  :setup setup
  :draw (partial draw (simple-test-proc))
  :size [500 300])
