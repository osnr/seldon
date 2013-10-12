(ns seldon.core)

(defrecord Tile [resources pops])

(defrecord Pop [memeome rates stocks])

(def meme->rates
  {:pastorialism
<<<<<<< HEAD
   (fn [value rates tile stocks]
     (assoc rates :fertility (+ (:fertility rates) 1)))

   :forest-gardening
   (fn [value rates tile stocks]
     rates)})
=======
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

    :bow-and-arrow (fn [value rates tile stocks] rates)
    :bronze-weapons (fn [value rates tile stocks] rates)
    :iron-weapons (fn [value rates tile stocks] rates)

    :slavery (fn [value rates tile stocks] (assoc rates :warlikeness (* 1.5 (rates :warlikeness)) ) )
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
    :human-sacrifice (fn [value rates tile stocks] (assoc rates :warlikeness (* 2.0 (rates :warlikeness))))

    :small-boats (fn [value rates tile stocks] rates)
    :sailing (fn [value rates tile stocks] rates)
    :paved-roads (fn [value rates tile stocks] rates)
    })
>>>>>>> rate update functions

(defn base-test []
  (let [base-pop
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

        base-tile
        (Tile. [] [base-pop])]
    base-tile))

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
  memeome)

(defn step-rates [tile pop]
  (let [memeome (:memeome pop)
        stocks (:stocks pop)
        rates (:rates pop)]
    (reduce-kv (fn [rates meme value]
                 ((meme->rates meme) value rates tile stocks))
               rates
               memeome)))

(defn step-pop [tile pop]
  (let [memeome (:memeome pop)
        stocks (:stocks pop)]
    (Pop. (step-memeome memeome)
          (step-rates tile pop)
          stocks)))

(defn step-tile [tile]
  (Tile. (:resources tile)
         (mapv (partial step-pop tile) (:pops tile))))

