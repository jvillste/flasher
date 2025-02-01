(ns hitype.times-table
  (:require
   [clojure.test :refer :all]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.layouts :as layouts]
   [flow-gl.graphics.font :as font]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [medley.core :as medley]))

(def maximum-exercise-points 3)

(def exercises (->> (for [x (range 2 10)
                          y (range 2 10)]
                      {:x x :y y})
                    (filter #(and (some  #{6} [(:x %) (:y %)])
                                  (<= (:x %)
                                      (:y %))))
                    ;;(take 1)
                    ))


(def tekstin-koko 40)

;; (def font (font/create-by-name "Serif" tekstin-koko))

(def dark-theme {:text-color [150 150 150 255]
                 :background-color [0 0 0 255]
                 :button-color [0 0 0 255]
                 :event-description-color [70 50 30]})

(def light-theme {:text-color [0 0 0 255]
                  :background-color [255 255 255 255]
                  :button-color [200 200 255 255]
                  :event-description-color [10 60 10]})
(def theme dark-theme)

(defn teksti [teksti & [koko väri]]
  (visuals/text-area (str teksti)
                     (if väri
                       väri
                       (:text-color theme))
                     (font/create-by-name "Serif" (or koko tekstin-koko))
                     #_(visuals/liberation-sans-regular (or koko tekstin-koko))))

(defn now []
  (System/currentTimeMillis))

(defn initialize-exercise [state exercise]
  (-> state
      (assoc :previous-exercise (:exercise state)
             :previous-options (:options state)
             :exercise-start-time (now)
             :show-numbers-the-other-way-around? (< 0.5 (rand))
             :exercise exercise
             :options (let [right-answer (* (:x exercise)
                                            (:y exercise))]
                        (->> (repeatedly (fn [] (max 2
                                                     (- (+ (inc (rand-int 20))
                                                           right-answer)
                                                        10))))
                             (remove #{right-answer})
                             (distinct)
                             (take 3)
                             (concat [right-answer])
                             (shuffle))))))



(defn next-exercise [state]
  (let [candidates (let [unfinished-exercises (remove (fn [exercise]
                                                        (<= maximum-exercise-points
                                                            (or (get-in state [:points exercise])
                                                                0)))
                                                      (:selected-exercises state))]
                     (if (= unfinished-exercises
                            [(:exercise state)])
                       [(:exercise state)]
                       (->> unfinished-exercises
                            (remove #{(:exercise state)})
                            (take 3))))]
    (when (not (empty? candidates))
      (rand-nth candidates))))

(def answer-keys [:a :s :d :f])

(def anwser-key-to-option-index (into {} (map vector answer-keys
                                              (range 4))))

(defonce points-atom (atom nil))

(comment
  (spit "lumon-time-table-points.edn" (pr-str @points-atom))
  )

(defn speed-points [duration]
  (int (min 5
            (max 0
                 (Math/floor (/ (- 6000
                                   duration)
                                1000))))))

(defn scores [state]
  (animation/swap-state! animation/set-wake-up 1000)
  (layouts/vertically-2 {:margin 10}
                        (teksti (str "Total answer points: " (apply + (vals (:points state)))))
                        (teksti (str "Total speed points: " (apply + (map (comp speed-points :duration)
                                                                          (filter :right-answer?
                                                                                  (:exercise-durations state))))))

                        (teksti (str "Right answers: " (count (filter :right-answer?
                                                                      (:exercise-durations state)))))
                        (teksti (str "Wrong answers: " (count (remove :right-answer?
                                                                      (:exercise-durations state)))))
                        (teksti (str "Passed execises: " (count (filter #(= % maximum-exercise-points)
                                                                        (vals (:points state))))))
                        (teksti (str "Time used: " (let [seconds (int (/ (- (if (:finished? state)
                                                                              (:previous-answer-time state)
                                                                              (now))
                                                                            (:start-time state))
                                                                         1000))]
                                                     (str (int (Math/floor (/ seconds 60)))
                                                          ":"
                                                          (mod seconds 60)))))))

(def answer-animation-duration 2000)

(defn- game-view  [state]
  (let [finish-phase (or (animation/phase! :finish)
                         0)

        wrong-answer-is-animating? (animation/animating? @animation/state-atom
                                                         :wrong-answer)]
    (layouts/superimpose (visuals/rectangle-2 :fill-color
                                              (:background-color theme))
                         (layouts/center-horizontally

                          (when (>= 1 finish-phase)
                            (layouts/with-margins (-> finish-phase
                                                      (animation/exponential-ease-in 3)
                                                      (animation/linear-mapping 0 400))
                              0 0 0
                              (layouts/with-margin 50
                                (layouts/vertically-2 {:margin 20 :centered? true}
                                                      (when (not (:finished? state))
                                                        (let [{:keys [x y]} (:exercise state)]
                                                          (teksti (if (:show-numbers-the-other-way-around? state)
                                                                    (str y " * " x)
                                                                    (str x " * " y)))))
                                                      (when (not (:finished? state))
                                                        (layouts/grid [(map (fn [value]
                                                                              (layouts/with-margin 10
                                                                                (teksti value tekstin-koko
                                                                                        (if wrong-answer-is-animating?
                                                                                          (let [right-answer (* (:x (:exercise state))
                                                                                                                (:y (:exercise state)))]
                                                                                            (cond (= right-answer value)
                                                                                                  [0 150 0 255]

                                                                                                  (= (:previous-answer state)
                                                                                                     value)
                                                                                                  [150 0 0 255]

                                                                                                  :else
                                                                                                  (:text-color theme)))
                                                                                          (:text-color theme)))))
                                                                            (:options state))
                                                                       (map (fn [anser-key]
                                                                              (layouts/with-margin 10 (teksti (name anser-key))))
                                                                            answer-keys)]))
                                                      (let [exercise-ponts-view (fn [exercise]
                                                                                  (layouts/with-maximum-size 190 nil
                                                                                    (let [points (or (get (:points state)
                                                                                                          exercise)
                                                                                                     0)
                                                                                          row-height 45
                                                                                          corner-radius 20
                                                                                          block (fn [fill-color]
                                                                                                  (layouts/box 5
                                                                                                               (visuals/rectangle-2 :fill-color nil
                                                                                                                                    :draw-color [80 80 80 255]
                                                                                                                                    :line-width 2
                                                                                                                                    :corner-arc-radius corner-radius)
                                                                                                               (assoc (visuals/rectangle-2 :fill-color fill-color
                                                                                                                                           :corner-arc-radius corner-radius)
                                                                                                                      :width 50
                                                                                                                      :height (- row-height
                                                                                                                                 10))))]
                                                                                      (layouts/superimpose (layouts/horizontally-2 {:margin 5}
                                                                                                                                   (concat (repeat (abs points)
                                                                                                                                                   (block (let [opacity (if (and (or (animation/animating? @animation/state-atom
                                                                                                                                                                                                           :right-answer)
                                                                                                                                                                                     (animation/animating? @animation/state-atom
                                                                                                                                                                                                           :wrong-answer))
                                                                                                                                                                                 (= (:previous-exercise state)
                                                                                                                                                                                    exercise))
                                                                                                                                                                          (animation/sine 0 255 0.4
                                                                                                                                                                                          (:time @animation/state-atom))
                                                                                                                                                                          255)]
                                                                                                                                                            (if (< 0 points)
                                                                                                                                                              [0 80 0 opacity]
                                                                                                                                                              [80 0 0 opacity]))))
                                                                                                                                           (repeat (- maximum-exercise-points
                                                                                                                                                      (abs points))
                                                                                                                                                   (block [0 0 0 0]))))
                                                                                                           (layouts/with-maximum-size nil row-height
                                                                                                             (layouts/center (teksti (str (:x exercise) " * " (:y exercise)))))))))]
                                                        (layouts/horizontally-2 {:margin 50}
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (take (/ (count (:selected-exercises state))
                                                                                                                              2)
                                                                                                                           (:selected-exercises state))]
                                                                                                        (exercise-ponts-view exercise)))
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (drop (/ (count (:selected-exercises state))
                                                                                                                              2)
                                                                                                                           (:selected-exercises state))]
                                                                                                        (exercise-ponts-view exercise)))))

                                                      (let [corner-radius 20
                                                            column-count (min (if (:finished? state)
                                                                                1000000
                                                                                20)
                                                                              (count (:exercise-durations state)))
                                                            width (min 50
                                                                       (/ 1000
                                                                          (max column-count
                                                                               1)))
                                                            block (fn [fill-color]
                                                                    (layouts/box 5
                                                                                 (visuals/rectangle-2 :fill-color nil
                                                                                                      :draw-color [80 80 80 255]
                                                                                                      :line-width (if (< width 10)
                                                                                                                    0
                                                                                                                    2)
                                                                                                      :corner-arc-radius corner-radius)
                                                                                 (assoc (visuals/rectangle-2 :fill-color fill-color
                                                                                                             :corner-arc-radius corner-radius)
                                                                                        :width width
                                                                                        :height 50)))]
                                                        (layouts/horizontally-2 {:margin (min 5
                                                                                              (/ 100
                                                                                                 (max column-count
                                                                                                      1)))
                                                                                 :end true}

                                                                                (for [exercise-duration (take-last column-count  (:exercise-durations state))]
                                                                                  (let [speed-points (speed-points (:duration exercise-duration))]
                                                                                    (layouts/vertically-2 {:margin 2}
                                                                                                          (concat (repeat (- 5 speed-points)
                                                                                                                          (block [0 0 0 0]))
                                                                                                                  (repeat speed-points
                                                                                                                          (block (if (:right-answer? exercise-duration)
                                                                                                                                   [0 80 0 255]
                                                                                                                                   [80 0 0 255])))))))
                                                                                (when (and (not (:finished? state))
                                                                                           (not wrong-answer-is-animating?))
                                                                                  (let [remaining-speed-points (speed-points (- (now)
                                                                                                                                (:exercise-start-time state)))]
                                                                                    (when (< 0 remaining-speed-points)
                                                                                      (animation/swap-state! animation/set-wake-up 100))
                                                                                    (layouts/vertically-2 {:margin 2}
                                                                                                          (concat (repeat (- 5 remaining-speed-points)
                                                                                                                          (block [0 0 0 0]))
                                                                                                                  (repeat remaining-speed-points
                                                                                                                          (block [0 80 0 255]))))))))
                                                      ;; (scores state)
                                                      )))))

                         (when (animation/running? @animation/state-atom :finish)
                           (layouts/center-horizontally
                            (assoc (scores state)
                                   :y (animation/linear-mapping (animation/exponential-ease-out finish-phase
                                                                                                3)
                                                                -1500
                                                                50))))

                         (when (animation/animating? @animation/state-atom
                                                     :right-answer
                                                     answer-animation-duration)
                           (layouts/center-horizontally
                            (let [phase (animation/phase! :right-answer)
                                  color [0 150 0 (- 255
                                                    (-> (animation/exponential-ease-in phase 2)
                                                        (animation/linear-mapping 0 155)))]

                                  y (- 400 (-> (animation/exponential-ease-out phase 3)
                                               (animation/linear-mapping 0 300)))]
                              (when (> 1 phase)
                                (layouts/superimpose (assoc (teksti "Right!" 50 color)
                                                            :x 0
                                                            :y y)
                                                     (assoc (teksti "Right!" 50 color)
                                                            :x 700
                                                            :y y))))))

                         (when (animation/animating? @animation/state-atom
                                                     :wrong-answer
                                                     answer-animation-duration)
                           (layouts/center-horizontally
                            (let [phase (animation/phase! :wrong-answer)
                                  color [150 0 0 (- 255
                                                    (-> (animation/exponential-ease-in phase 2)
                                                        (animation/linear-mapping 0 155)))]

                                  y (+ 100 (-> (animation/exponential-ease-out phase 3)
                                               (animation/linear-mapping 0 300)))]
                              (when (> 1 phase)
                                (layouts/superimpose (assoc (teksti "Wrong!" 50 color)
                                                            :x 0
                                                            :y y)
                                                     (assoc (teksti "Wrong!" 50 color)
                                                            :x 700
                                                            :y y)))))))))

(defn reset-game-state [state]
  (assoc state
         :finished? false
         :exercise nil
         :exercise-durations []
         :start-time (now)
         :points {}))

(defn initialize-state []
  (let [state {:selected-exercises #{}
               :exercise-durations []
               :start-time (now)
               :state :menu
               } #_{:points (read-string (slurp "lumon-time-table-points.edn"))}
        ]
    state))

(def all-exercises (->> (for [x (range 2 10)
                              y (range 2 10)]

                          {:x x :y y})
                        (filter #(<= (:x %)
                                     (:y %)))))

(defn button [label fill-color text-color on-click!]
  {:node (layouts/box 10
                      (visuals/rectangle-2 :fill-color fill-color
                                           :corner-arc-radius 20)
                      (teksti label tekstin-koko text-color))
   :mouse-event-handler (fn [_node event]
                          (when (= :mouse-clicked (:type event))
                            (on-click!))
                          event)})

(defn exercise-score [durations exercise]
  (let [relevant-durations (->> durations
                                (filter (fn [duration]
                                          (= exercise (:exercise duration))))
                                (sort-by :time)
                                (take-last 10))]
    (float (/ (reduce +
                      (map (comp speed-points :duration)
                           (filter :right-answer? relevant-durations)))
              10))))

(defn average-exercise-duration-in-seconds [durations exercise]
  (let [relevant-durations (->> durations
                                (filter (fn [duration]
                                          (= exercise (:exercise duration))))
                                (sort-by :time)
                                (take-last 10))]
    (float (/ (+ (reduce +
                         (map (fn [duration]
                                (min 5000 (:duration duration)))
                              (filter :right-answer? relevant-durations)))
                 (* 5000 (- 10 (count (filter :right-answer? relevant-durations)))))
              10
              1000))))

(deftest test-average-exercise-duration-in-seconds
  (is (= 5.0
         (average-exercise-duration-in-seconds []
                                               {:x 2 :y 2})))

  (is (= 4.5
         (average-exercise-duration-in-seconds [{:exercise {:x 2 :y 2}
                                                 :right-answer? true
                                                 :duration 0}]
                                               {:x 2 :y 2})))
  (is (= 5.0
         (average-exercise-duration-in-seconds [{:exercise {:x 2 :y 2}
                                                 :right-answer? true
                                                 :duration 10000}]
                                               {:x 2 :y 2}))))

(defn start-game [state-atom]
  (when (not (empty? (:selected-exercises @state-atom)))
    (swap! state-atom (fn [state]
                        (let [state (reset-game-state state)]
                          (-> (initialize-exercise state
                                                   (next-exercise state))
                              (assoc :state :game)))))))

(defn clear-selected-exercises [state-atom]
  (swap! state-atom assoc :selected-exercises #{}))


(defn add-random-exercise [state-atom]
  (swap! state-atom (fn [state]
                      (update state :selected-exercises
                              set/union
                              (if-let [new-exercise (dissoc (->> all-exercises
                                                                 (remove (set (:selected-exercises state)))
                                                                 (map (fn [exercise]
                                                                        (assoc exercise
                                                                               :sorting-value [(Math/round (* 5 (average-exercise-duration-in-seconds (get-in state [:players (:player state) :history])
                                                                                                                                                      exercise)))

                                                                                               (rand)])))
                                                                 (sort-by :sorting-value)
                                                                 (last))
                                                            :sorting-value)]
                                #{new-exercise}
                                #{})))))

(defn toggle-average-druation [state-atom]
  (swap! state-atom update :exercise-rendering (fn [rendering]
                                                 (if (= rendering :average-duration)
                                                   nil
                                                   :average-duration))))

(defn toggle-right-answer [state-atom]
  (swap! state-atom update :exercise-rendering (fn [rendering]
                                                 (if (= rendering :right-answer)
                                                   nil
                                                   :right-answer))))

(comment
  (def numbers (take 10 (repeatedly rand)))
  (for [multiplier (range 1 100)]
    (let [groups (vals (group-by (fn [number]
                                   (Math/round (* multiplier number)))
                                 numbers))]
      [multiplier
       (count groups)
       ;; (->> groups
       ;;      (sort-by first)
       ;;      (map (fn [group]
       ;;             (map (fn [number]
       ;;                    [number (Math/round (* multiplier number))])
       ;;                  group))))
       ]))
  ) ;; TODO: remove me


(defn change-to-next-player [state-atom]
  (swap! state-atom assoc :player (first (remove #{(:player @state-atom)}
                                                 (sort (keys (:players @state-atom)))))))

(defn menu-view [state-atom]
  (let [state @state-atom
        player-history (get-in state [:players (:player state) :history])]
    (layouts/with-margin 50
      (layouts/center-horizontally
       (layouts/vertically-2 {:margin 50 :centered? true}
                             (layouts/horizontally-2 {:margin 20}
                                                     (for [player-id (keys (:players state))]
                                                       (button (get-in state [:players player-id :name])
                                                               (if (= player-id (:player state))
                                                                 [50 180 50 255]
                                                                 [10 10 10 255])
                                                               (if (= player-id (:player state))
                                                                 [0 0 0 255]
                                                                 [150 150 150 255])
                                                               (fn []
                                                                 (swap! state-atom assoc :player player-id)))))
                             (layouts/grid (for [row (partition-by :x
                                                                   all-exercises)]
                                             (for [exercise row]

                                               {:node (let [selected? (contains? (:selected-exercises state)
                                                                                 exercise)]
                                                        (layouts/with-margin 5
                                                          (layouts/box 5
                                                                       (visuals/rectangle-2 :fill-color (if selected?
                                                                                                          [50 180 50 255]
                                                                                                          [0 0 (* (max 0
                                                                                                                       (min 1
                                                                                                                            (/ (- 5 (average-exercise-duration-in-seconds player-history exercise))
                                                                                                                               5)))
                                                                                                                  255)
                                                                                                           255])
                                                                                            :corner-arc-radius 20)
                                                                       (layouts/with-minimum-size 100 nil
                                                                         (layouts/with-maximum-size 100 nil
                                                                           (layouts/center-horizontally
                                                                            (teksti (case (:exercise-rendering state)

                                                                                      :average-duration
                                                                                      (format "%.2f" (average-exercise-duration-in-seconds player-history exercise))

                                                                                      :right-answer
                                                                                      (str (* (:x exercise) (:y exercise)))

                                                                                      (str (:x exercise) " * " (:y exercise) ))
                                                                                    tekstin-koko
                                                                                    (if selected?
                                                                                      [0 0 0 255]
                                                                                      [200 200 200 255]))))))))
                                                :mouse-event-handler (fn [_node event]
                                                                       (when (= :mouse-clicked (:type event))
                                                                         (let [similar-exercises (filter (fn [available-exercise]
                                                                                                           (or (= (:x exercise)
                                                                                                                  (:x available-exercise))
                                                                                                               (= (:x exercise)
                                                                                                                  (:y available-exercise))))
                                                                                                         all-exercises)]
                                                                           (swap! state-atom update :selected-exercises (fn [selected-exericses]
                                                                                                                          (if (contains? selected-exericses
                                                                                                                                         exercise)
                                                                                                                            (if (:shift event)
                                                                                                                              (apply disj
                                                                                                                                     selected-exericses
                                                                                                                                     similar-exercises)
                                                                                                                              (disj selected-exericses
                                                                                                                                    exercise))
                                                                                                                            (if (:shift event)
                                                                                                                              (apply conj
                                                                                                                                     selected-exericses
                                                                                                                                     similar-exercises)
                                                                                                                              (conj selected-exericses
                                                                                                                                    exercise)))))))
                                                                       event)})))
                             (teksti (str "Average duration: " (format "%.2f"
                                                                       (/ (->> (map (partial average-exercise-duration-in-seconds player-history)
                                                                                    all-exercises)
                                                                               (reduce +))
                                                                          (count all-exercises)))
                                          "s")
                                     60
                                     [50 180 50 255])
                             (layouts/horizontally-2 {:margin 10}
                                                     [button "Clear selection (e)"
                                                      [50 50 50 255]
                                                      [180 180 180 255]
                                                      (fn []
                                                        (clear-selected-exercises state-atom))]
                                                     [button "Add random (r)"
                                                      [50 50 50 255]
                                                      [180 180 180 255]
                                                      (fn []
                                                        (add-random-exercise state-atom))]
                                                     [button "Toggle average duration (x)"
                                                      [50 50 50 255]
                                                      [180 180 180 255]
                                                      (fn []
                                                        (toggle-average-druation state-atom))]

                                                     [button "Toggle right answer (g)"
                                                      [50 50 50 255]
                                                      [180 180 180 255]
                                                      (fn []
                                                        (toggle-right-answer state-atom))]
                                                     [button "Play! (space)"
                                                      [50 50 50 255]
                                                      (if (empty? (:selected-exercises @state-atom))
                                                        [100 100 100 255]
                                                        [180 180 180 255])
                                                      (fn [] (start-game state-atom))]))))))

(def state-file-name "times-table-state.edn")

(defn select-last-durations-of-each-exercise [durations]
  (->> (group-by :exercise durations)
       (medley/map-vals (fn [single-exercise-durations]
                          (->> single-exercise-durations
                               (sort-by :time)
                               (take-last 10))))
       (vals)
       (apply concat)))

(defn save-game [state]
  (let [state (update-in state [:players (:player state) :history] (fn [history]
                                                                     (select-last-durations-of-each-exercise (concat (or history [])
                                                                                                                     (:exercise-durations state)))))]
    (spit state-file-name
          (prn-str state))
    state))

(defn event-handler [state-atom _node event]
  (let [state @state-atom]
    (when (and (= :key-pressed (:type event))
               (= :escape (:key event)))
      (swap! state-atom save-game)
      (swap! state-atom assoc :state :menu))

    (when (= :menu (:state state))
      (when (and (= :key-pressed (:type event))
                 (= :x (:key event)))
        (toggle-average-druation state-atom))

      (when (and (= :key-pressed (:type event))
                 (= :g (:key event)))
        (toggle-right-answer state-atom))

      (when (and (= :key-pressed (:type event))
                 (= :space (:key event)))
        (start-game state-atom))

      (when (and (= :key-pressed (:type event))
                 (= :e (:key event)))
        (clear-selected-exercises state-atom))

      (when (and (= :key-pressed (:type event))
                 (= :r (:key event)))
        (if (:shift? event)
          (doseq [_ (range 5)]
            (add-random-exercise state-atom))
          (add-random-exercise state-atom)))

      (when (and (= :key-pressed (:type event))
                 (= :tab (:key event)))
        (change-to-next-player state-atom)))

    (when (= :game (:state state))
      (if (and (:finished? state)
               (= :key-pressed (:type event)))
        (do (animation/swap-state! animation/delete :finish)
            (swap! state-atom save-game)
            (swap! state-atom assoc :state :menu))
        (when (and (= :key-pressed (:type event))
                   (some #{(:key event)}
                         answer-keys)
                   (not (animation/animating? @animation/state-atom :wrong-answer answer-animation-duration)))

          (let [answer (get (:options state)
                            (anwser-key-to-option-index (:key event)))
                right-answer? (= (* (:x (:exercise state))
                                    (:y (:exercise state)))
                                 answer)
                state (update-in state
                                 [:points (:exercise state)]
                                 (fn [points]
                                   (let [points (or points 0)
                                         points (if right-answer?
                                                  (inc points)
                                                  (dec points))]
                                     (-> points
                                         (max (- maximum-exercise-points))
                                         (min maximum-exercise-points)))))
                state (update state :exercise-durations conj
                              {:exercise (:exercise state)
                               :time (now)
                               :duration (- (now)
                                            (:exercise-start-time state))
                               :right-answer? right-answer?})
                next-exercise (next-exercise state)
                finished? (not (some? next-exercise))]

            (reset! state-atom
                    (-> state
                        (assoc :finished? finished?
                               :previous-answer-time (now)
                               :previous-answer answer)
                        (cond-> (and (some? next-exercise)
                                     right-answer?)
                          (initialize-exercise next-exercise))))

            (if finished?
              (animation/swap-state! animation/start :finish 2000)
              (if right-answer?
                (animation/swap-state! animation/start :right-answer answer-animation-duration)
                (do (animation/swap-state! animation/add-animation-end-callback :wrong-answer (fn []
                                                                                                (when (some? next-exercise)
                                                                                                  (swap! state-atom initialize-exercise next-exercise))))
                    (animation/swap-state! animation/start :wrong-answer answer-animation-duration))))

            (reset! points-atom
                    (:points @state-atom))))))))

(defn root-view []
  (let [state-atom (atom (assoc (read-string (slurp state-file-name))
                                :state :menu)
                         #_{:selected-exercises #{}
                            :state :menu
                            :players {1 {:name "Lumo"}
                                      2 {:name "Jukka"}}
                            :player 1})]
    (fn []
      (let [state @state-atom]
        (keyboard/set-focused-event-handler! (partial event-handler state-atom))

        (case (:state state)
          :game (game-view state)
          :menu (menu-view state-atom))))))

(defonce event-channel-atom (atom nil))

(defn start []
  #_(aloita)

  (reset! event-channel-atom
          (application/start-application #'root-view
                                         :on-exit #(reset! event-channel-atom nil))))


(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))
