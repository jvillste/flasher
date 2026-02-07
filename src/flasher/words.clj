(ns flasher.words
  (:require
   [clojure.edn :as edn]
   [flasher.continuous-times-table :as continuous-times-table]
   [flasher.times-table :as times-table]
   [fungl.application :as application]))

(defn word-excercises [words]
  (concat (for [word words]
            (assoc word
                   :question-language :language-a
                   :type :word))

          (for [word words]
            (assoc word
                   :question-language :language-b
                   :type :word))))

(def words-atom (atom []))

(defn options [answer-language right-answer]
  (->> @words-atom
       (remove (comp #{right-answer} answer-language))
       (shuffle)
       (take 3)
       (map answer-language)
       (concat [right-answer])
       (shuffle)))

(defn answer-languate [exercise]
  (case (:question-language exercise)
    :language-a :language-b
    :language-b :language-a))

(defmethod times-table/exercise-attributes :word [exercise]
  {:question ((:question-language exercise) exercise)
   :answer ((answer-languate exercise)
            exercise)
   :group (dissoc exercise :question-language)
   :options-function (partial options (answer-languate exercise))})

(defn game-view [language-a-key language-b-key word-file-name]
  (let [words (map (fn [word]
                     {:language-a (language-a-key word)
                      :language-b (language-b-key word)})
                   (edn/read-string (slurp (str "temp/" word-file-name ".edn"))))
        word-excercises-list (word-excercises words)]
    (reset! words-atom words)
    (fn []
      [continuous-times-table/game-view
       (str "temp/" word-file-name "-state.edn")
       word-excercises-list
       (constantly false)
       {:rating-mode :points}])))

(defn english-game-view [word-file-name]
  (game-view :english :finnish word-file-name))

(defn swedish-game-view [word-file-name]
  (game-view :swedish :finnish word-file-name))

(def the-game-view
  #_(english-game-view "come-with-me-5-chapter-8")
  (swedish-game-view "trampolin-3-page-106"))

(application/def-start the-game-view)
