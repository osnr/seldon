(ns seldon.draw
  (:require [seldon.core :refer [simple-test]]
            [clojure.core.async :refer [go <!!]]
            [quil.core :refer :all]))

(def avg-radius 30)

(defn pop->circle [pop i tile-x tile-y]
  (let [memome (:memome pop)]
    {:x        (+ tile-x avg-radius (* avg-radius i))
     :y        (+ tile-y avg-radius)
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
  (let [tile (<!! in)]
    (background 255)
    (let [idv (map vector (iterate inc 0)
                   (:pops tile))]
      (doseq [[i pop] idv]
        (draw-circle (pop->circle pop i 0 0))))))

(defsketch seldon
  :title "Seldon viewer"
  :setup setup
  :draw (partial draw (simple-test))
  :size [500 300])
