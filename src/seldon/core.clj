(ns seldon.core)

(defrecord Tile [resources pops])

(defrecord Pop [memeome rates stocks])

(def base-rates
  {:fertility 10
   :mortality 0.5
   :pop-stability 1
   :cultural-spread 0.5
   :political-stability 1
   :warlikeness 0.5
   :gdp 0.01})

(def meme->rates
  {:pastorialism
   (fn [value rates tile stocks]
     (assoc rates :fertility (+ (:fertility rates) 1)))

   :forest-gardening
   (fn [value rates tile stocks]
     rates)})

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
               :cultural-spread 0.1
               :political-stability 0.1
               :warlikeness 0.1
               :gdp 1}

              {:population 100
               :domestication-corn 0
               :domestication-potatoes 0
               :weapons-bow-and-arrow 0

               :deforestation 0})

        big-tile
        (Tile. {:forest 1
                :bronze 0.5
                :iron 0.5
                :cropland 0.75}
               [big-pop])]
    big-tile))

(defn simple-test []
  (let [simple-pop (Pop. {:pastorialism 1
                          :forest-gardening 2}

                         {:fertility 100
                          :mortality 200}

                         {:population 10
                          :domestication-corn 0})
        simple-tile (Tile. [] [simple-pop])]
    (loop [tile simple-tile]
      (clojure.pprint/pprint tile)
      (Thread/sleep 500)
      (recur (step-tile tile)))))

(defn step-memeome [memeome]
  (into {} (for [[meme value] memeome]
             [meme (+ value (rand-normal (/ 1 60)))])))

(defn step-rates [tile pop]
  (let [memeome (:memeome pop)
        stocks (:stocks pop)
        rates (:rates pop)]
    (reduce-kv (fn [rates meme value]
                 ((meme->rates meme) value rates tile stocks))
               base-rates
               memeome)))

(defn step-pop [tile pop]
  (let [memeome (:memeome pop)
        stocks (:stocks pop)]
    (Pop. (step-memeome memeome)
          (step-rates tile pop)
          stocks)))

(defn step-tile [tile]
  ; TODO (calculate state of tile based on stocks, regrowth rates, etc)
  (Tile. (:resources tile)
         (mapv (partial step-pop tile) (:pops tile))))

