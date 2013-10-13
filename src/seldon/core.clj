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

   :forest-change 0
   :cropland-change 0})

(defn agriculturalProductivity [tile]
  (tile :cropland))

(def meme->rates
  {:pastorialism
   (fn [value rates tile stocks]
     (assoc rates :food-production (+ (rates :food-production)
                                      (* value (stocks :population) 4))
                  :war-preparation (+ (* value 2) (rates :war-preparation))))

   :forest-gardening
   (fn [value rates tile stocks]
     (assoc rates
       :food-production (+ (rates :food-production)
                           (* value (stocks :population) 2))))

   :agriculture (fn [value rates tile stocks] 
     (let [oldforest ((tile :resources) :forest)
           forestch (+ (rates :forest-change) (* value (stocks :population) -0.01))
           newforest (+ oldforest forestch)
           croplandch (max (- oldforest newforest) 0)]
       (assoc rates
         :food-production (+ (rates :food-production) (* value (stocks :population) 8))
         :forest-change forestch
         :cropland-change croplandch)))

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
     (assoc rates :warlikeness (* value 2.0 (rates :warlikeness))))})

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
     (assoc resources :forest (+ (resources :forest)
                                 rate-value)))

   :cropland-change
   (fn [rate-value resources]
     (assoc resources :cropland (+ (resources :cropland)
                                   rate-value)))})

(defn rand-normal [scale]
  (* scale
     (- (reduce + (take 12 (repeatedly rand))) 6)))

(defn big-test []
  (let [big-pop
        (Pop. {:pastorialism 0.1
               :forest-gardening 0.1
               :agriculture 0.1
               :slash-and-burn 0.1
               :irrigation 0.1
               :crop-rotation 0.1

               :pseudo-writing 0.1
               :writing 0.1
               :alphabetic-writing 0.1

               :bow-and-arrow 0.1
               :bronze-weapons 0.1
               :iron-weapons 0.1

               :slavery 0.1
               :class-structure 0.1
               :cities 0.1

               :bazaar-economy 0.1
               :state-planning 0.1
               :value-gold 0.1
               :value-silver 0.1

               :shamanic 0.1
               :pantheon-of-gods 0.1
               :one-god 0.1
               :revelation 0.1
               :state-priesthood 0.1
               :holy-sites 0.1
               :animal-sacrifice 0.1
               :human-sacrifice 0.1

               :small-boats 0.1
               :sailing 0.1
               :paved-roads 0.1}

              {:fertility 1
               :mortality 1
               :pop-stability 1
               :political-stability 0.1
               :warlikeness 0.1
               :gdp 1
               :war-preparation 0.1
               :forest-change 0
               :cropland-change 0}

              {:population 100
               :war-readiness 0.1
               :wealth 0
               })

        big-tile
        (Tile. {:forest 1
                :elevation 0.5
                :aridity 0.5
                :bronze 0.5
                :iron 0.5
                :cropland 0.25
                :wetness 1}
               [big-pop])]
    big-tile))

(defn step-memome [tile memome]
  (into {} (for [[meme value] memome]
             [meme (max 0
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

(defn step-resources [resources pops]
  (reduce (fn [resources pop]
            (reduce-kv (fn [resources rate rate-value]
                         ((rate->resources rate (constantly resources))
                          rate-value resources))
                       resources
                       (:rates pop)))
          resources
          pops))

(defn diffuse-pop [pop target-pop]
  (let [pop-weight (:population (:stocks pop))
        target-pop-weight (:population (:stocks target-pop))
        pop-memome (:memome pop)
        target-pop-memome (:memome target-pop)]
    (Pop. (into {} (map #(vector (first %1)
                                 (weighted-average (second %1)
                                                   (second %2)
                                                   pop-weight
                                                   target-pop-weight))
                        (:memome pop) (:memome target-pop)))
          (:rates target-pop)
          (:stocks target-pop))))

(defn diffuse-pops [pops]
  (reduce (fn [target-pops pop]
            (map (fn [target-pop]
                   (if (= target-pop pop)
                     pop
                     (diffuse-pop pop target-pop)))
                 target-pops))
          pops
          pops))

(defn step-tile [tile]
  (let [pops (remove #(= 0 (:population (:stocks %))) (:pops tile))]
    (Tile. (step-resources (:resources tile) pops)
           (diffuse-pops (mapv (partial step-pop tile) pops)))))

(defn step-map [map]
  )

(defn simple-test-proc []
  (let [simple-pop (Pop. {:pastorialism 0.1 ; simple memes
                          :forest-gardening 0.2
                          :bow-and-arrow 0.1}

                         base-rates ; simple rates

                         {:population 10 ; simple stocks
                          :war-readiness 0.1
                          :wealth 0})

        simple-pop-2 (-> simple-pop
                         (assoc-in [:memome :pastorialism] 0.9)
                         (assoc-in [:memome :forest-gardening] 0.9)
                         (assoc-in [:memome :bow-and-arrow] 0.1))

        simple-pop-3 (assoc-in simple-pop [:memome :pastorialism] 0.5)

        simple-pop-4 (-> simple-pop
                         (assoc-in [:stocks :population] 20)
                         (assoc-in [:memome :pastorialism] 0.01)
                         (assoc-in [:memome :forest-gardening] 0.01))

        simple-tile (Tile. {:forest 1
                            :elevation 0.5
                            :aridity 0.5
                            :bronze 0.5
                            :iron 0.5
                            :cropland 0.75}
                           [simple-pop simple-pop-2 simple-pop-3 simple-pop-4])

        simple-map (vec (repeat 3 (vec (repeat 3 simple-tile))))

        out (chan)]
    (go (loop [tile simple-tile]
          (>! out tile)
          (clojure.pprint/pprint tile)
          (Thread/sleep 500)
          (recur (step-tile tile))))
    out))

(defn -main []
  (let [in (simple-test-proc)]
    (while true
      (<!! in))))
