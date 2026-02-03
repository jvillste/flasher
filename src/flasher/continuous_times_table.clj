(ns flasher.continuous-times-table
  (:require
   [clojure.test :refer :all]
   [flasher.random :as random]
   [flasher.times-table :as times-table]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.color :as color]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [medley.core :as medley])
  (:import
   (java.io File)))

(def minimum-duration 2500)
(def maximum-duration 5000)
(def candidate-exercise-count 6)

(defn exercise-state [state exercise]
  (get-in state
          [:players (:player state) :exercises exercise]))

(def day-in-milliseconds (* 1000 60 60 24))

(def time-interval-in-milliseconds-after-repetition {1 (* 1 day-in-milliseconds)
                                                     2 (* 2 day-in-milliseconds)
                                                     3 (* 4 day-in-milliseconds)
                                                     ;; 4 (* 8 day-in-milliseconds)
                                                     })

(def maximum-repetition (inc (apply max (keys time-interval-in-milliseconds-after-repetition))))

(defn average-duration [state exercise]
  (get (exercise-state state exercise)
       :average-duration
       maximum-duration))

(defn repetition [state exercise]
  (get (exercise-state state exercise)
       :repetition
       0))

(defn can-be-repeated? [state exercise]
  (let [repetition-number (repetition state exercise)]
    (or (= 0 repetition-number)
        (and (> maximum-repetition repetition-number)
             (< (+ (time-interval-in-milliseconds-after-repetition repetition-number)
                   (:last-completion-time (exercise-state state exercise)))
                (times-table/now))))))

(defn group-familarity [player-exercises]
  (medley/map-vals (fn [exercise-statuses]
                     (/ (reduce + (map (fn [[_exercise status]]
                                         (or (:average-duration status)
                                             maximum-duration))
                                       exercise-statuses))
                        (count exercise-statuses)))
                   (group-by (comp :group times-table/exercise-attributes first)
                             player-exercises)))

(deftest test-group-familarity
  (is (= {{:type :multiplication, :lower-number 2, :higher-number 3} 2000,
          {:type :multiplication, :lower-number 3, :higher-number 5} 5000}
         (group-familarity {{:type :multiplication, :x 2, :y 3} {:average-duration 1000}
                            {:type :multiplication, :x 3, :y 2} {:average-duration 2000}
                            {:type :division, :x 2, :y 3} {:average-duration 3000}
                            {:type :division, :x 5, :y 3} {:average-duration 5000}}))))

(defn close-numbers? [x1 x2 y1 y2]
  (and (= x1
          x2)
       (or (= y1
              (dec y2))
           (= y1
              (inc y2)))))

(defn similar-numbers? [exercises exercise]
  (some (fn [familiar-exercise]
          (or (close-numbers? (:x exercise)
                              (:x familiar-exercise)
                              (:y exercise)
                              (:y familiar-exercise))
              (close-numbers? (:y exercise)
                              (:y familiar-exercise)
                              (:x exercise)
                              (:x familiar-exercise))))
        exercises))

(defn next-exercise-candidates [exercises similar-exercise? state]
  (let [player-exercise-statuses (get-in state [:players (:player state) :exercises])
        current-exercises (->> (keys player-exercise-statuses)
                               (filter (partial can-be-repeated? state))
                               (sort-by (fn [exercise]
                                          [(- maximum-repetition
                                              (repetition state exercise))
                                           (average-duration state exercise)])))
        group-familiarity-map (group-familarity player-exercise-statuses)
        exercises-in-familiar-groups (->> exercises
                                          (filter (partial can-be-repeated? state))
                                          (remove (set current-exercises))
                                          (remove (comp nil? group-familiarity-map :group times-table/exercise-attributes))
                                          (sort-by (comp group-familiarity-map :group times-table/exercise-attributes)))
        exercises-with-familiar-numbers (->> exercises
                                             (filter (partial can-be-repeated? state))
                                             (remove (set current-exercises))
                                             (remove (set exercises-in-familiar-groups))
                                             (filter (partial similar-exercise?
                                                              (concat current-exercises
                                                                      exercises-in-familiar-groups)))
                                             (random/shuffle-collection))]
    (->> (concat current-exercises
                 exercises-in-familiar-groups
                 exercises-with-familiar-numbers)
         (remove (fn [exercise]
                   (= (:exercise state)
                      exercise))))))

(deftest test-next-exercise-candidates
  (is (= '({:type :multiplication, :x 4, :y 2}
           {:y 4, :type :multiplication, :x 3}
           {:y 4, :type :division, :x 3}
           {:y 3, :type :division, :x 4}
           {:y 4, :type :multiplication, :x 2}
           {:y 4, :type :division, :x 2}
           {:y 2, :type :division, :x 4}
           {:y 2, :type :division, :x 3}
           {:y 4, :type :multiplication, :x 4}
           {:y 3, :type :multiplication, :x 2}
           {:y 5, :type :division, :x 3}
           {:y 3, :type :multiplication, :x 3}
           {:y 3, :type :division, :x 2}
           {:y 2, :type :multiplication, :x 5}
           {:y 3, :type :multiplication, :x 5}
           {:y 2, :type :division, :x 5}
           {:y 5, :type :multiplication, :x 3}
           {:y 3, :type :division, :x 5}
           {:y 2, :type :multiplication, :x 3}
           {:y 5, :type :division, :x 2}
           {:y 5, :type :multiplication, :x 2}
           {:y 4, :type :division, :x 4}
           {:y 3, :type :division, :x 3})
         (with-redefs [times-table/now (constantly 5000)
                       time-interval-in-milliseconds-after-repetition {1 1000}]
           (random/with-fixed-random-seed
             (next-exercise-candidates times-table/exercises
                                       similar-numbers?
                                       {:players {1 {:exercises {{:type :multiplication, :x 4, :y 3} {:average-duration 3000}
                                                                 {:type :multiplication, :x 4, :y 2} {:average-duration 5000
                                                                                                      :repetition 1
                                                                                                      :last-completion-time 0}}}},
                                        :player 1
                                        :exercise {:type :multiplication, :x 4, :y 3}}))))))

(defn next-exercise [exercises similar-exercise? state]
  (let [next-exercise-candidates-list (next-exercise-candidates exercises similar-exercise? state)]
    (->> (concat next-exercise-candidates-list
                 (->> exercises
                      (remove (set next-exercise-candidates-list))
                      (random/shuffle-collection)))
         (take candidate-exercise-count)
         (random/pick-random))))

(def initial-state {:players {1 {:name "Lumo"}
                              2 {:name "Jukka"}
                              3 {:name "Valo"}}
                    :player 1
                    :exercise-rendering :question})


(def state-file-name "temp/continuous-times-table-state.edn")

(defn save-game! [state-file-name state]
  (spit state-file-name
        (prn-str state)))

(comment
  (spit state-file-name
        (prn-str initial-state))
  )

(defn keyboard-event-handler [exercises similar-exercise? state-file-name state-atom _node event]
  (when (and (= :key-pressed (:type event))
             (some #{(:key event)}
                   times-table/answer-keys)
             (not (animation/animating? @animation/state-atom
                                        :wrong-answer
                                        times-table/answer-animation-duration)))

    (let [state @state-atom
          answer (get (vec (:options state))
                      (times-table/anwser-key-to-option-index (:key event)))
          right-answer? (= (:answer (times-table/exercise-attributes (:exercise state)))
                           answer)
          next-exercise (next-exercise exercises similar-exercise? state)]

      (swap! state-atom assoc :answer answer)

      (when (can-be-repeated? state (:exercise state))
        (swap! state-atom
               update-in
               [:players (:player state) :exercises (:exercise state)]
               (fn [exercise-state]
                 (let [new-average-duration (double (/ (+ (if right-answer?
                                                            (min maximum-duration
                                                                 (- (times-table/now)
                                                                    (:exercise-start-time state)))
                                                            maximum-duration)
                                                          (get-in state
                                                                  [:players (:player state) :exercises (:exercise state) :average-duration]
                                                                  maximum-duration))
                                                       2))
                       exercise-state (or exercise-state
                                          {:repetition 0})]
                   (cond (< new-average-duration
                            minimum-duration)
                         (assoc exercise-state
                                :average-duration maximum-duration
                                :repetition (min maximum-repetition
                                                 (inc (repetition state (:exercise state))))
                                :last-completion-time (times-table/now))

                         right-answer?
                         (assoc exercise-state
                                :average-duration new-average-duration)

                         :else
                         exercise-state)))))

      (if right-answer?
        (do (swap! state-atom times-table/initialize-exercise next-exercise)
            (save-game! state-file-name @state-atom)
            (animation/swap-state! animation/start :right-answer times-table/answer-animation-duration))
        (do (animation/swap-state! animation/add-animation-end-callback :wrong-answer (fn []
                                                                                        (swap! state-atom times-table/initialize-exercise next-exercise)))
            (animation/swap-state! animation/start :wrong-answer times-table/answer-animation-duration)))))

  (when (and (= :key-pressed (:type event))
             (= :e (:key event)))
    (swap! state-atom update :exercise-rendering (fn [exercise-rendering]
                                                   (case exercise-rendering
                                                     :right-answer
                                                     :question

                                                     :question
                                                     :right-answer

                                                     :question))))

  (when (and (= :key-pressed (:type event))
             (= :r (:key event)))
    (swap! state-atom update :exercise-rendering (fn [exercise-rendering]
                                                   (case exercise-rendering
                                                     :average-duration
                                                     :question

                                                     :question
                                                     :average-duration

                                                     :question))))

  #_(when (and (= :key-pressed (:type event))
               (= :space (:key event)))
      (swap! state-atom update :exercise-rendering (fn [exercise-rendering]
                                                     (case exercise-rendering
                                                       :average-duration
                                                       :right-answer

                                                       :right-answer
                                                       :question

                                                       :question
                                                       :average-duration))))

  (when (and (= :key-pressed (:type event))
             (= :tab (:key event)))
    (times-table/change-to-next-player state-atom)))

(def ready-color times-table/green #_(conj (color/hsluv-to-rgb 135 1.0 0.4) 1.0))
(def almost-ready-color (conj (color/hsluv-to-rgb 67 1.0 0.4) 1.0))
(def unready-color (conj (color/hsluv-to-rgb 0 0.0 0.4) 1.0))

(defn duration-color [duration]
  (if (< duration minimum-duration)
    ready-color
    unready-color))

(defn duration-cell [exercise duration]
  (let [width (* 6 times-table/tekstin-koko)]
    (layouts/with-margin 10
      (layouts/with-minimum-size width nil
        (layouts/with-maximum-size nil 20
          (layouts/center-vertically
           (layouts/superimpose (assoc (visuals/rectangle-2 {:fill-color (duration-color duration)})
                                       :height times-table/tekstin-koko
                                       :width (* width
                                                 (abs (/ duration
                                                         maximum-duration))))
                                (times-table/teksti (str (:question (times-table/exercise-attributes exercise))
                                                         ":" duration)
                                                    {:color [200 200 200 255]}))))))))

(defn exericse-grid [exercises state]
  (layouts/grid (for [row (partition 8
                                     #_(comp first :related-numbers)
                                     (map (fn [exercise]
                                            (assoc (times-table/exercise-attributes exercise)
                                                   :exercise exercise))
                                          exercises))]
                  (for [attributes row]
                    {:node (let [width 250]
                             (layouts/with-margin 5
                               (layouts/vertically-2 {:margin 10 :centered? true}
                                                     (layouts/box 5
                                                                  (visuals/rectangle-2 :fill-color  (if (can-be-repeated? state (:exercise attributes))
                                                                                                      [0
                                                                                                       0
                                                                                                       (float (max 0
                                                                                                                   (min 1
                                                                                                                        (/ (- maximum-duration
                                                                                                                              (average-duration state (:exercise attributes)))
                                                                                                                           minimum-duration))))
                                                                                                       255]
                                                                                                      ready-color)
                                                                                       :corner-arc-radius 20)
                                                                  (layouts/with-minimum-size width nil
                                                                    (layouts/with-maximum-size width nil
                                                                      (layouts/center-horizontally
                                                                       (times-table/teksti (case (:exercise-rendering state)

                                                                                             :average-duration
                                                                                             (str (format "%.2f" (float (/ (average-duration state (:exercise attributes))
                                                                                                                           1000))))

                                                                                             :right-answer
                                                                                             (:answer attributes)

                                                                                             (:question attributes))
                                                                                           times-table/tekstin-koko
                                                                                           [200 200 200 255])))))

                                                     (layouts/with-minimum-size nil 10
                                                       (layouts/horizontally-2 {}
                                                                               (repeat (repetition state (:exercise attributes))
                                                                                       (assoc (visuals/rectangle-2 :fill-color ready-color
                                                                                                                   :draw-color [80 80 80 255]
                                                                                                                   :line-width 2
                                                                                                                   :corner-arc-radius 20)
                                                                                              :width (/ width maximum-repetition)
                                                                                              :height 15)))))))}))))

(defn add-repetition [exercise-state]
  (if (< (:average-duration exercise-state)
         minimum-duration)
    (assoc exercise-state
           :repetition 1
           :average-duration maximum-duration)
    (assoc exercise-state
           :repetition 0)))

(deftest test-add-repetition
  (is (= {:average-duration 5000,
          :last-completion-time 1767360073925,
          :repetition 1}
         (add-repetition {:average-duration 2229.3100810050964,
                          :last-completion-time 1767360073925})))

  (is (= {:average-duration 3000,
          :last-completion-time 1767360073925,
          :repetition 0}
         (add-repetition {:average-duration 3000
                          :last-completion-time 1767360073925}))))

(defn set-last-completion-time [exercise-state]
  (dissoc (if (= 1 (:repetition exercise-state))
            (assoc exercise-state
                   :last-completion-time (:last-right-answer-time exercise-state))
            exercise-state)
          :last-right-answer-time))

(comment
  (#_save-game! (-> (read-string (slurp state-file-name))
                  (update-in [:players 1 :exercises] (partial medley/map-vals set-last-completion-time))
                  (update-in [:players 2 :exercises] (partial medley/map-vals set-last-completion-time))
                  (update-in [:players 3 :exercises] (partial medley/map-vals set-last-completion-time))))
  )

(defn game-view  [state-file-name exercises similar-exercise?]
  (let [state-atom (dependable-atom/atom (if (.exists (File. state-file-name))
                                           (read-string (slurp state-file-name))
                                           initial-state))]
    (swap! state-atom times-table/initialize-exercise (next-exercise exercises similar-exercise? @state-atom))

    (fn [_state-file-name _exercises similar-exercise?]
      (let [wrong-answer-is-animating? (animation/animating? @animation/state-atom
                                                             :wrong-answer)
            state @state-atom]
        {:node (layouts/superimpose (visuals/rectangle-2 :fill-color (:background-color times-table/theme))

                                    (layouts/center-horizontally (layouts/vertically-2 {:centered? true}
                                                                                       (layouts/with-margins 50 0 50 0
                                                                                         (layouts/horizontally-2 {:margin 20}
                                                                                                                 (for [player-id (keys (:players state))]
                                                                                                                   (times-table/button (get-in state [:players player-id :name])
                                                                                                                                       (if (= player-id (:player state))
                                                                                                                                         [50 180 50 255]
                                                                                                                                         [10 10 10 255])
                                                                                                                                       (if (= player-id (:player state))
                                                                                                                                         [0 0 0 255]
                                                                                                                                         [150 150 150 255])
                                                                                                                                       (fn []
                                                                                                                                         (swap! state-atom assoc :player player-id))))))
                                                                                       (layouts/with-margins 50 0 50 0
                                                                                         (times-table/teksti (:question (times-table/exercise-attributes (:exercise state)))))

                                                                                       (times-table/options-view wrong-answer-is-animating?
                                                                                                                 state)
                                                                                       (layouts/with-margins 50 0 50 0
                                                                                         [exericse-grid exercises state])

                                                                                       (times-table/teksti (str "Number of completed repetitions: " (->> (get-in state [:players (:player state) :exercises])
                                                                                                                                                         (vals)
                                                                                                                                                         (map :repetition)
                                                                                                                                                         (reduce +))))

                                                                                       (times-table/teksti "e: toggle answers, r: toggle average durations")


                                                                                       #_(layouts/with-maximum-size 1000 nil
                                                                                           (layouts/flow (let [exercises-to-average-durations (exercises-to-average-durations state)]
                                                                                                           (for [exercise times-table/exercises]
                                                                                                             (duration-cell exercise (get exercises-to-average-durations
                                                                                                                                          exercise
                                                                                                                                          maximum-duration))))))))



                                    (when (animation/animating? @animation/state-atom
                                                                :right-answer
                                                                times-table/answer-animation-duration)
                                      (layouts/center-horizontally
                                       (let [phase (animation/phase! :right-answer)
                                             color [0 150 0 (- 255
                                                               (-> (animation/exponential-ease-in phase 2)
                                                                   (animation/linear-mapping 0 155)))]

                                             y (- 400 (-> (animation/exponential-ease-out phase 3)
                                                          (animation/linear-mapping 0 300)))]
                                         (when (> 1 phase)
                                           (layouts/superimpose (assoc (times-table/teksti "Right!" 50 color)
                                                                       :x 0
                                                                       :y y)
                                                                (assoc (times-table/teksti "Right!" 50 color)
                                                                       :x 700
                                                                       :y y))))))

                                    (when (animation/animating? @animation/state-atom
                                                                :wrong-answer
                                                                times-table/answer-animation-duration)
                                      (layouts/center-horizontally
                                       (let [phase (animation/phase! :wrong-answer)
                                             color [150 0 0 (- 255
                                                               (-> (animation/exponential-ease-in phase 2)
                                                                   (animation/linear-mapping 0 155)))]

                                             y (+ 100 (-> (animation/exponential-ease-out phase 3)
                                                          (animation/linear-mapping 0 300)))]
                                         (when (> 1 phase)
                                           (layouts/superimpose (assoc (times-table/teksti "Wrong!" 50 color)
                                                                       :x 0
                                                                       :y y)
                                                                (assoc (times-table/teksti "Wrong!" 50 color)
                                                                       :x 700
                                                                       :y y)))))))
         :keyboard-event-handler [keyboard-event-handler exercises similar-exercise? state-file-name state-atom]
         :can-gain-focus? true}))))

(defn times-table-game-view []
  [game-view
   state-file-name
   times-table/exercises
   similar-numbers?])

(application/def-start times-table-game-view)

(comment
  (reset! event-channel-atom nil)
  )
