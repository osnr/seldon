(ns seldon.core)

(defrecord Tile [resources pops])

(defrecord Pop [memeome rates stocks])

(def meme->rates
  {:pastorialism
   (fn [meme rates tile stocks]
     )

   :forest-gardening
   (fn [meme rates tile stocks]
     )})

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

               :slavery-+war 0.1
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
               :weapons-bow-and-arrow 0})

        base-tile
        (Tile. [] [base-pop])]
    base-tile))

