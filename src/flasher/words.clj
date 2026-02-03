(ns flasher.words
  (:require
   [clojure.edn :as edn]
   [flasher.continuous-times-table :as continuous-times-table]
   [flasher.times-table :as times-table]
   [fungl.application :as application]))

(def state-file-name "temp/level-up-7-page-96-state.edn")

(defn english-word-to-word [word]
  {:language-a (:english word)
   :language-b (:finnish word)})

(def level-up-7-page-96-words (map english-word-to-word (edn/read-string (slurp "temp/level-up-7-page-96.edn"))))

(defn word-excercises [words]
  (concat (for [word words]
            (assoc word
                   :question-language :language-a
                   :type :word))

          (for [word words]
            (assoc word
                   :question-language :language-b
                   :type :word))
          ;; (for [word words]
          ;;   {:question (:finnish word)
          ;;    :answer (:english word),
          ;;    :type :word
          ;;    :group {:type :word
          ;;            :english (:english word)
          ;;            :finnish (:finnish word)},
          ;;    :options-function (partial options words :finnish)})
          ))

(defn options [words language right-answer]
  (->> words
       (remove #{right-answer})
       (shuffle)
       (take 3)
       (map language)
       (concat [right-answer])
       (shuffle)))

(defn answer-languate [exercise]
  (case (:question-language exercise)
    :language-a :language-b
    :language-b :language-a))

(def words (map english-word-to-word (edn/read-string (slurp "temp/level-up-7-page-96.edn"))))

(defmethod times-table/exercise-attributes :word [exercise]
  {:question ((:question-language exercise) exercise)
   :answer ((answer-languate exercise)
            exercise)
   :group (dissoc exercise :question-language)
   :options-function (partial options words (answer-languate exercise))}
  #_exercise)

(def game-view
  (let [word-excercises-list (word-excercises (#_drop take 25 words))]
    (fn []
      [continuous-times-table/game-view
       "temp/level-up-7-page-96-state.edn"
       word-excercises-list
       (constantly false)])))

(defn english-game-view [word-file-name]
  (let [word-excercises-list (word-excercises (map english-word-to-word (edn/read-string (slurp (str "temp/" word-file-name ".edn")))))]
    (fn []
      [continuous-times-table/game-view
       (str "temp/" word-file-name "-state.edn")
       word-excercises-list
       (constantly false)])))

(def the-english-game-view (english-game-view "come-with-me-5-chapter-8"))

#_(application/def-start game-view)

(application/def-start the-english-game-view)

(comment
  ((:options-function (times-table/exercise-attributes (first (word-excercises (take 25 (map english-word-to-word (edn/read-string (slurp "temp/level-up-7-page-96.edn"))))))))
   "foo")

  (count (edn/read-string (slurp "temp/come-with-me-5-page-108.edn")))

  )
