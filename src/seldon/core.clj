(ns seldon.core
  (:require [clojure.core.async :refer [>! <!! go chan]]))

(defrecord Tile [resources pops])

(defrecord Pop [memome rates stocks])

(defn weighted-average [a b a-weight b-weight]
  (/ (+ (* a a-weight) (* b b-weight)) (+ a-weight b-weight)))

(def base-rates
  {:fertility 2.0
   :mortality 0.5

   :pop-stability 1
   :political-stability 1

   :warlikeness 0.5
   :war-preparation 0.1

   :food-production 0
   :gdp 0.01

   :forest-change 0.000
   :cropland-change 0
   :pasture-change 0})

(defn agriculturalProductivity [tile]
  ((:resources tile) :cropland))

(defn pastoralProductivity [tile]
  ((:resources tile) :pasture))

(defn gardeningProductivity [tile]
  ((:resources tile) :forest))

(def meme->rates
  {:pastorialism
   (fn [value rates tile stocks]
     (let [oldcropland ((:resources tile) :cropland)
           croplandch (+ (rates :cropland-change) (* value (stocks :population) -0.0001))
           newcropland (+ oldcropland croplandch)
           pasturech (max (- oldcropland newcropland) 0)]
     (assoc rates :food-production (+ (rates :food-production)
                                      (min (* value 2000 (pastoralProductivity tile)) (* value (stocks :population) 4 (pastoralProductivity tile))))
                  :war-preparation (+ (* value 2) (rates :war-preparation))
                  :cropland-change croplandch
                  :pasture-change (* -1 croplandch))))

   :forest-gardening
   (fn [value rates tile stocks]
     (assoc rates
       :food-production (+ (rates :food-production)
                           (min (* value 1000 (gardeningProductivity tile)) (* value (stocks :population) 2 (gardeningProductivity tile))))))

   :agriculture (fn [value rates tile stocks] 
     (let [oldforest ((:resources tile) :forest)
           forestch (+ (rates :forest-change) (* value (stocks :population) -0.00005))
           newforest (+ oldforest forestch)
           croplandch (max (- oldforest newforest) 0)
           pasturech (+ (rates :pasture-change) (* value (stocks :population) -0.0001))]
       (assoc rates
         :food-production (+ (rates :food-production) 
                             (min (* value 20000 (agriculturalProductivity tile)) (* value (stocks :population) 8 (agriculturalProductivity tile))))
         :forest-change forestch
         :cropland-change (- (- 0 forestch) pasturech)
         :pasture-change pasturech)))

   :slash-and-burn (fn [value rates tile stocks] 
     (assoc rates :food-production (+ (rates :food-production)
                                      (* value (stocks :population) 4))))
   :hunter-gatherer (fn [value rates tile stocks]
     (assoc rates :food-production (+ (rates :food-production)
                                      (* value (stocks :population) 1))
            :war-preparation (+ (* value 1)
                                (rates :war-preparation))))

   :bow-and-arrow
   (fn [value rates tile stocks]
     (assoc rates :war-preparation (+ (* value 1.5) (rates :war-preparation))))
   :bronze-weapons
   (fn [value rates tile stocks]
     (assoc rates :war-preparation (+ (* value 2.0) (rates :war-preparation))))
   :iron-weapons
   (fn [value rates tile stocks]
     (assoc rates :war-preparation (+ (* value 2.0) (rates :war-preparation))))

   :slavery
   (fn [value rates tile stocks]
     (assoc rates :warlikeness (* value 1.5 (rates :warlikeness))))

   :human-sacrifice
   (fn [value rates tile stocks]
     (assoc rates :warlikeness (* value 2.0 (rates :warlikeness))))
   
   :fertility-modifier
   (fn [value rates tile stocks]
     (assoc rates :fertility (* (/ 1 value) (rates :fertility))))})

(def rate->stocks
  {:fertility
   (fn [rate-value stocks]
     (assoc stocks :population (+ (stocks :population)
                                  (* rate-value (/ (stocks :population) 2) ))))

   :mortality
   (fn [rate-value stocks]
     (assoc stocks :population (* (- 1 rate-value)
                                  (stocks :population)) ))

   :pop-stability
   (fn [rate-value stocks]
     stocks) ; TODO add 'outcasts' to stocks

   :political-stability
   (fn [rate-value stocks]
     stocks) ; TODO ??

   :warlikeness
   (fn [rate-value stocks]
     stocks) ; leads up to war (chance of war)

   :gdp
   (fn [rate-value stocks]
     stocks) ; TODO 'wealth' stock

   :war-preparation
   (fn [rate-value stocks]
     (assoc stocks :war-readiness (+ rate-value
                                     (stocks :war-readiness))))
   :food-production
   (fn [rate-value stocks]
     (let [dif (- rate-value (stocks :population))]
     (assoc stocks :population (if (< dif 0)
                                 (+ (stocks :population) dif)
                                 (stocks :population))
                   :wealth (if (> dif 0)
                             (+ (stocks :wealth) dif)
                             (stocks :wealth)))))})

(def rate->resources
  {:forest-change
   (fn [rate-value resources]
     (assoc resources :forest (max 0
                                   (min 1
                                        (+ (resources :forest)
                                           rate-value)))))

   :cropland-change
   (fn [rate-value resources]
     (assoc resources :cropland (max 0
                                     (min 1
                                          (+ (resources :cropland)
                                             rate-value)))))
   
   :pasture-change
   (fn [rate-value resources]
     (assoc resources :pasture (max 0
                                    (min 1
                                         (+ (resources :pasture)
                                            rate-value)))))})

(defn rand-normal [scale]
  (* scale
     (- (reduce + (take 12 (repeatedly rand))) 6)))

(defn step-memome [tile memome]
  (into {} (for [[meme value] memome]
             [meme (max 0.001
                        (min 1
                             (+ value (rand-normal (/ 1.0 60.0)))))])))

(defn step-rates [tile memome pop]
  (let [stocks (:stocks pop)
        rates (:rates pop)]
    (reduce-kv (fn [rates meme value]
                 ((meme->rates meme (constantly rates))
                  value rates tile stocks))
               base-rates
               memome)))

(defn step-stocks [rates stocks]
  (reduce-kv (fn [stocks rate rate-value]
               ((rate->stocks rate (constantly stocks))
                rate-value stocks))
             stocks
             rates))

(defn step-pop [tile pop]
  (let [memome (step-memome tile (:memome pop))
        rates (step-rates tile memome pop)
        stocks (step-stocks rates (:stocks pop))]
    (Pop. memome
          rates
          stocks)))

(defn constrain-resources [resources]
  (let [land-sum (+ (:pasture resources) (:forest resources) (:cropland resources))
        scale-pasture (/ (:pasture resources) land-sum)
        scale-forest (/ (:forest resources) land-sum)
        scale-cropland (/ (:cropland resources) land-sum)]
    (assoc resources :pasture scale-pasture :forest scale-forest :cropland scale-cropland)))

(defn step-resources [resources pops]
  (constrain-resources (reduce (fn [resources pop]
            (reduce-kv (fn [resources rate rate-value]
                         ((rate->resources rate (constantly resources))
                          rate-value resources))
                       resources
                       (:rates pop)))
          resources
          pops)))

(defn step-tile [tile]
  (let [pops (remove #(= 0 (:population (:stocks %))) (:pops tile))]
    (Tile. (step-resources (:resources tile) pops)
           (mapv (partial step-pop tile) pops))))

(defn diffuse-pop [pop target-pop]
  (let [pop-weight (:population (:stocks pop))
        target-pop-weight (:population (:stocks target-pop))
        pop-memome (:memome pop)
        target-pop-memome (:memome target-pop)]
    (Pop. (into {} (map #(vector (first %1)
                                 (weighted-average (second %1)
                                                   (second %2)
                                                    pop-weight 
                                                   (Math/pow target-pop-weight 5)))
                        (:memome pop) (:memome target-pop)))
          (:rates target-pop)
          (:stocks target-pop))))

(defn diffuse-grid-pop [grid [x y pop]]
  (reduce (fn [grid path]
            (let [target-tile (get-in grid path)
                  target-pops (:pops target-tile)]
              (assoc-in grid path
                        (assoc target-tile
                          :pops (mapv (fn [target-pop]
                                        (if (or (= target-pop pop)
                                                (and (not= path [x y])
                                                     (< (rand) 0.5)))
                                          target-pop
                                          (diffuse-pop pop target-pop)))
                                      target-pops)))))
          grid
          (for [dx [-1 0 1]
                dy [-1 0 1]
                :let [path [(+ x dx) (+ y dy)]]
                :when (get-in grid path)]
            path)))

(defn diffuse-grid-pops [grid]
  (reduce diffuse-grid-pop
          grid
          (apply concat (for [x (range 0 (count grid))
                              y (range 0 (count (first grid)))]
                          (for [pop (:pops (get-in grid [x y]))]
                            [x y pop])))))

(defn step-grid [grid]
  (time (diffuse-grid-pops (mapv (partial mapv step-tile) grid))))

(defn simple-test-proc []
  (let [simple-pop (Pop. {:pastorialism 0.75 ; simple memes
                          :forest-gardening 0.5
                          :agriculture 0.25
                          :fertility-modifier 0.3}

                         base-rates ; simple rates

                         {:population 200 ; simple stocks
                          :war-readiness 0.1
                          :wealth 0})

        simple-tile (Tile. {:forest 0.0
                            :elevation 0.5
                            :aridity 0.5
                            :bronze 0.5
                            :iron 0.5
                            :cropland 0.25
                            :pasture 0.75}
                           (vec (repeat 4 simple-pop)))

        simple-grid (vec (repeat 9 (vec (repeat 9 simple-tile))))

        out (chan)]
    (go (loop [grid simple-grid]
          (>! out grid)
          (Thread/sleep 100)
          (recur (step-grid grid))))
    out))

(defn -main []
  (let [in (simple-test-proc)]
    (while true
      (<!! in))))
