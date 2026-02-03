(ns flasher.sanaluokat
  (:require
   [flasher.continuous-times-table :as continuous-times-table]
   [flasher.times-table :as times-table]
   [fungl.application :as application]))

(def sanaluokat {"persoonapronomini" ["minä" "sinä" "hän" "me" "te" "he"]
                 "osoituspronomini" ["tämä" "tuo" "ne" "nämä" "nuo" "ne"]
                 "kysyvä pronomini" ["kuka" "mikä" "kumpi"]
                 "relatiivipronomini" ["joka" "mikä"]
                 "järjestysluku" ["ensimmäinen" "toinen" "kolmas" "neljäs"]
                 "perusluku" ["yksi" "kaksi" "kolme"]
                 "werbi" ["katso" "kokea" "ajaa" "juosta"]
                 "adjektiivi" ["hieno" "kaunis" "pehmeä"]
                 "konjunktio" ["että" "jotta" "koska" "kun" "jos" "vaikka" "ja" "sekä" "eli" "tai" "vai" "mutta" "sillä" "vaan"]
                 "adverbi" ["nyt" "kohta" "eilen"
                            "alas" "ylös" "takana" "edessä"
                            "hienosti" "nopeasti" "kierrellen" "komeasti"
                            "paljon" "vähän"]})

(def exercises (for [sanaluokka (keys sanaluokat)
                     sana (get sanaluokat sanaluokka)]
                 {:sana sana
                  :sanaluokka sanaluokka
                  :type :sanaluokka}))

(defn options [right-answer]
  (->> (keys sanaluokat)
       (remove #{right-answer})
       (shuffle)
       (take 3)
       (concat [right-answer])
       (shuffle)))

(defmethod times-table/exercise-attributes :sanaluokka [exercise]
  {:question (:sana exercise)
   :answer (:sanaluokka exercise)
   :group (:sanaluokka exercise)
   :options-function options})

(defn game-view []
  [continuous-times-table/game-view
   "temp/sanaluokat-state.edn"
   exercises
   (constantly false)])

(application/def-start game-view)
