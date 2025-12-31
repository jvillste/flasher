(ns flasher.continuous-times-table
  (:require
   [clojure.test :refer :all]
   [flasher.times-table :as times-table]
   [fungl.color :as color]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [medley.core :as medley])
  (:import
   (java.io File)))

(def minimum-duration 2500)
(def maximum-duration 5000)
(def candidate-exercise-count 4)

(defn average-duration [state exercise]
  (get-in state
          [:players (:player state) :exercises exercise :average-duration]
          maximum-duration))

(defn next-exercise [state]
  (let [current-exercises (->> (get-in state [:players (:player state) :exercises])
                               (remove (fn [[exercise status]]
                                         (or (= (:exercise state)
                                                exercise)
                                             (< (:average-duration status)
                                                minimum-duration))))
                               (sort-by (fn [[_exercise status]]
                                          (:average-duration status)))
                               (take candidate-exercise-count)
                               (map first))]
    (->> current-exercises
         (concat (take (max 0
                            (- candidate-exercise-count
                               (count current-exercises)))
                       (->> times-table/exercises
                            (remove (fn [exercise]
                                      (or (= (:exercise state)
                                             exercise)
                                          (< (average-duration state exercise)
                                             minimum-duration))))
                            (shuffle))))
         (rand-nth))))


(def initial-state {:players {1 {:name "Lumo"}
                              2 {:name "Jukka"}}
                    :player 1
                    :exercise-rendering :question})


(def state-file-name "continuous-times-table-state.edn")

(defn save-game! [state]
  (spit state-file-name
        (prn-str state)))

(defn keyboard-event-handler [state-atom _node event]
  (when (and (= :key-pressed (:type event))
             (some #{(:key event)}
                   times-table/answer-keys)
             (not (animation/animating? @animation/state-atom
                                        :wrong-answer
                                        times-table/answer-animation-duration)))

    (let [state @state-atom
          right-answer? (= (:answer (times-table/exercise-attributes (:exercise state)))
                           (get (vec (:options state))
                                (times-table/anwser-key-to-option-index (:key event))))
          next-exercise (next-exercise state)]

      (swap! state-atom
             assoc-in
             [:players (:player state) :exercises (:exercise state)]
             (merge {:average-duration (/ (+ (if right-answer?
                                               (min maximum-duration
                                                    (- (times-table/now)
                                                       (:exercise-start-time state)))
                                               maximum-duration)
                                             (get-in state
                                                     [:players (:player state) :exercises (:exercise state) :average-duration]
                                                     maximum-duration))
                                          2)}
                    (when right-answer?
                      {:last-right-answer-time (times-table/now)})))

      (if right-answer?
        (do (swap! state-atom times-table/initialize-exercise next-exercise)
            (save-game! @state-atom)
            (animation/swap-state! animation/start :right-answer times-table/answer-animation-duration))
        (do (animation/swap-state! animation/add-animation-end-callback :wrong-answer (fn []
                                                                                        (swap! state-atom times-table/initialize-exercise next-exercise)))
            (animation/swap-state! animation/start :wrong-answer times-table/answer-animation-duration)))))


  (when (and (= :key-pressed (:type event))
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

(def ready-color (conj (color/hsluv-to-rgb 135 1.0 0.4) 1.0))
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

(defn exericse-grid [state]
  (layouts/grid (for [row (partition-by (comp first :related-numbers)
                                        (map times-table/exercise-attributes times-table/exercises))]
                  (for [attributes row]

                    {:node (layouts/with-margin 5
                             (layouts/box 5
                                          (visuals/rectangle-2 :fill-color  (if (< (average-duration state (:exercise attributes))
                                                                                   minimum-duration)
                                                                              ready-color
                                                                              [0
                                                                               0
                                                                               (* (max 0
                                                                                       (min 1
                                                                                            (/ (- maximum-duration (average-duration state (:exercise attributes)))
                                                                                               maximum-duration)))
                                                                                  255)
                                                                               255])
                                                               :corner-arc-radius 20)
                                          (layouts/with-minimum-size 150 nil
                                            (layouts/with-maximum-size 150 nil
                                              (layouts/center-horizontally
                                               (times-table/teksti (case (:exercise-rendering state)

                                                                     :average-duration
                                                                     (str (format "%.2f" (float (/ (average-duration state (:exercise attributes))
                                                                                                   1000))))

                                                                     :right-answer
                                                                     (:answer attributes)

                                                                     (:question attributes))
                                                                   times-table/tekstin-koko
                                                                   [200 200 200 255]))))))}))))

(defn- game-view  []
  (let [state-atom (dependable-atom/atom (if (.exists (File. state-file-name))
                                           (read-string (slurp state-file-name))
                                           initial-state))]
    (swap! state-atom times-table/initialize-exercise (next-exercise @state-atom))
    (fn []
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
                                                                                         [exericse-grid state])

                                                                                       (times-table/teksti (str "Number of ready exercises: " (->> (get-in state [:players (:player state) :exercises])
                                                                                                                                                   (vals)
                                                                                                                                                   (filter (fn [exercise]
                                                                                                                                                             (< (:average-duration exercise)
                                                                                                                                                                minimum-duration)))
                                                                                                                                                   (count))))


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
         :keyboard-event-handler [keyboard-event-handler state-atom]
         :can-gain-focus? true}))))


(application/def-start game-view)
