(ns seldon.core)

(defrecord Tile [resources pops])

(defrecord Pop [memome rates stocks])

(def base-rates
  {:fertility 10
   :mortality 0.5
   :pop-stability 1
   :cultural-contact 0.5
   :political-stability 1
   :warlikeness 0.5
   :gdp 0.01})

(def meme->rates
  {:pastorialism
   (fn [value rates tile stocks]
     rates)

   :forest-gardening
   (fn [value rates tile stocks]
     rates)
   :agriculture (fn [value rates tile stocks] rates)
   :slash-and-burn (fn [value rates tile stocks] rates)
   :irrigation (fn [value rates tile stocks] rates)
   :crop-rotation (fn [value rates tile stocks] rates)

   :pseudo-writing (fn [value rates tile stocks] rates)
   :writing (fn [value rates tile stocks] rates)
   :alphabetic-writing (fn [value rates tile stocks] rates)

   :bow-and-arrow (fn [value rates tile stocks] (assoc rates :war-readiness (* value 1.5 (rates :war-readiness))))
   :bronze-weapons (fn [value rates tile stocks] (assoc rates :war-readiness (* value 2.0 (rates :war-readiness))))
   :iron-weapons (fn [value rates tile stocks] (assoc rates :war-readiness (* value 2.0 (rates :war-readiness))))

   :slavery (fn [value rates tile stocks] (assoc rates :warlikeness (* value 1.5 (rates :warlikeness)) ) )
   :class-structure (fn [value rates tile stocks] rates)
   :cities (fn [value rates tile stocks] rates)

   :bazaar-economy (fn [value rates tile stocks] rates)
   :state-planning (fn [value rates tile stocks] rates)
   :value-gold (fn [value rates tile stocks] rates)
   :value-silver (fn [value rates tile stocks] rates)

   :shamanic (fn [value rates tile stocks] rates)
   :pantheon-of-gods (fn [value rates tile stocks] rates)
   :one-god (fn [value rates tile stocks] rates)
   :revelation (fn [value rates tile stocks] rates)
   :state-priesthood (fn [value rates tile stocks] rates)
   :holy-sites (fn [value rates tile stocks] rates)
   :animal-sacrifice (fn [value rates tile stocks] rates)
   :human-sacrifice (fn [value rates tile stocks] (assoc rates :warlikeness (* value 2.0 (rates :warlikeness))))

   :small-boats (fn [value rates tile stocks] rates)
   :sailing (fn [value rates tile stocks] rates)
   :paved-roads (fn [value rates tile stocks] rates)})

(def rate->stocks
  {:fertility
   (fn [rate-value stocks]
     stocks)

   :mortality
   (fn [rate-value stocks]
     stocks)

   :pop-stability
   (fn [rate-value stocks]
     stocks)

   :cultural-contact
   (fn [rate-value stocks]
     stocks)

   :political-stability
   (fn [rate-value stocks]
     stocks)

   :warlikeness
   (fn [rate-value stocks]
     stocks)

   :gdp
   (fn [rate-value stocks]
     stocks)})

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
               :cultural-contact 0.1
               :political-stability 0.1
               :warlikeness 0.1
               :gdp 1}

              {:population 100
               :war-readiness 0
               :deforestation 0})

        big-tile
        (Tile. {:forest 1
                :bronze 0.5
                :iron 0.5
                :cropland 0.75}
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
                 ((meme->rates meme) value rates tile stocks))
               base-rates
               memome)))

(defn step-stocks [rates stocks]
  (reduce-kv (fn [stocks rate rate-value]
               ((rate->stocks rate) rate-value stocks))
             stocks
             rates))

(defn step-pop [tile pop]
  (let [memome (step-memome tile (:memome pop))
        rates (step-rates tile memome pop)
        stocks (step-stocks rates (:stocks pop))]
    (Pop. memome
          rates
          stocks)))

(defn step-tile [tile]
  ; TODO (calculate state of tile based on stocks, regrowth rates, etc)
  (Tile. (:resources tile)
         (mapv (partial step-pop tile) (:pops tile))))

(defn simple-test []
  (let [simple-pop (Pop. {:pastorialism 0.1 ; simple memes
                          :forest-gardening 0.2}

                         base-rates ; simple rates

                         {:population 10 ; simple stocks
                          :domestication-corn 0})
        simple-tile (Tile. [] [simple-pop])]
    (loop [tile simple-tile]
      (clojure.pprint/pprint tile)
      (Thread/sleep 500)
      (recur (step-tile tile)))))
