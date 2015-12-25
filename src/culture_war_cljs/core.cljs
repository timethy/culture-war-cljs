(ns culture-war-cljs.core
  (:require [cljs.core.async :refer [chan close! >! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros
    [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(enable-console-print!)

;; (repl/connect "http://localhost:9000/repl")

;; Core code here:

(defprotocol ICultureWar
  (update-color [this v c])
  (color-of [this v])
  (neighbours-of [this v])
  (cell-ids [this]))

(def ^:private neighbouring-coords
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not= [0 0] [i j])] [i j]))

(defn- get-in-grid [grid i j]
  (get (get grid i) j))

(defrecord IGridCultureWar [wrap-around m n grid cell-ids]
  ICultureWar
  (update-color [this v c] (assoc this :grid (update-in grid v (constantly c))))
  (color-of [this [i j]] (get-in-grid grid i j))
  (neighbours-of [this [i j]]
    (let [[dec-m dec-n inc-m inc-n] (if wrap-around
                                      [(fn [x] (mod (dec x) m))
                                       (fn [x] (mod (dec x) n))
                                       (fn [x] (mod (inc x) m))
                                       (fn [x] (mod (inc x) n))]
                                      [dec dec inc inc])]
      (filter some? [(get-in-grid grid i (dec-n j))
                     (get-in-grid grid i (inc-n j))
                     (get-in-grid grid (dec-m i) (dec-n j))
                     (get-in-grid grid (dec-m i) j)
                     (get-in-grid grid (dec-m i) (inc-n j))
                     (get-in-grid grid (inc-m i) (dec-n j))
                     (get-in-grid grid (inc-m i) j)
                     (get-in-grid grid (inc-m i) (inc-n j))])))
  (cell-ids [this] cell-ids))

(defn grid-culture-war [m n wrap-around init-fn]
  (let [grid (mapv #(mapv (partial init-fn %) (range n)) (range m))]
    (map->IGridCultureWar {:m m :n n :wrap-around wrap-around :grid grid :cell-ids (for [i (range m) j (range n)] [i j])})))

(defprotocol IElection
  (majority [this own-color neighbours] "returns a [IMajority object major color]"))

(defn max-by [f coll]
  (reduce (fn [a b] (if (>= (f a) (f b)) a b)) coll))

; Make this faster?
(defn majors-in [neighbours]
  (let [freqs (frequencies neighbours)
        max-freq (second (max-by second freqs))]
    (map first (filter (comp (partial = max-freq) second) freqs))))

(def conservative-election
  (reify IElection
    (majority [this own-color neighbours]
      (let [[m-1 & m-rest] (majors-in neighbours)]
        (if m-rest own-color
          m-1)))))

(defn random-election [random]
  (reify IElection
    (majority [this own-color neighbours]
      (let [majors (majors-in neighbours)]
        (nth majors (random (count majors)))))))

(defn update-war [war election]
  (let [ids (cell-ids war)]
    (reduce (fn [old-war id]
              (update-color old-war id (majority election (color-of war id) (neighbours-of war id))))
      war ids)))

(defn run-war [existing-wars election steps]
  (reduce
    (fn [wars i]
      (conj wars (update-war (peek wars) election)))
    existing-wars (range steps)))

;; Rendering control code here

(def default-settings
  {:m 10
   :n 10
   :c 10
   :wrap-around false
   :election :random
   :time-used 0})

(defn initial-war [{:keys [m n c wrap-around]}]
  (grid-culture-war m n wrap-around (fn [i j] (rand-int c))))

(defn empty-war [{:keys [m n c wrap-around]}]
  (grid-culture-war m n wrap-around (constantly 0)))

(defonce app-state (atom {:wars [(empty-war default-settings)]
                          :playing? false
                          :position 0
                          :new-steps 1
                          :settings default-settings
                          :new-settings default-settings}))

; Copied from d3js
(def d3js-colors ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"])
(defn greyscale [n]
  (let [stepsize (int (/ 256 n))]
    (for [i (range n)]
      (apply str "#" (repeat 3 (.toString (* i stepsize) 16))))))

(defn color-strings [n]
  (if (<= n 10)
    (take n d3js-colors)
    (greyscale n)))

(defn- cell-style [c] #js {:backgroundColor c})

(defn render-grid [colors m n grid]
  (for [i (range m)]
    (dom/div #js {:className "cw-row"}
      (for [j (range n)]
        (dom/span #js {:className "cw-cell"
                       :style (cell-style (colors (get-in grid [i j])))}
          ".")))))

(defn jump-to [n] (swap! app-state update-in [:position] (constantly n)))
(defn jump-by [n] (swap! app-state (fn [{:keys [position wars] :as args}]
                                     (assoc args :position (-> position (+ n) (max 0) (min (dec (count wars))))))))

(defn reinit []
  (swap! app-state (fn [{:keys [new-settings] :as args}]
                     (assoc args
                       :new-steps 1
                       :wars [(initial-war new-settings)]
                       :settings new-settings
                       :position 0))))

(defn populate [n]
  (swap! app-state
    (fn [{:keys [wars position settings] :as state}]
      (let [start-time (.getTime (js/Date.))
            war-count (count wars)
            diff (- n war-count)
            election (if (= (settings :election) :random)
                       (random-election rand-int)
                       conservative-election)]
        (if (pos? diff)
          (-> state
            (assoc
              :wars (run-war wars election diff)
              :position (+ position diff))
            (assoc
              :time-used (- (.getTime (js/Date.)) start-time)))
          state)))))

(defn render-controls [war-count]
  (dom/div #js {:className "cw-controls"}
    (dom/button #js {:onClick (partial jump-to 0)} "|<")
    (dom/button #js {:onClick (partial jump-by -100)} "<100")
    (dom/button #js {:onClick (partial jump-by -10)} "<10")
    (dom/button #js {:onClick (partial jump-by -1)} "<")
    (dom/button nil ".")
    (dom/button #js {:onClick (partial jump-by +1)} ">")
    (dom/button #js {:onClick (partial jump-by +10)} ">10")
    (dom/button #js {:onClick (partial jump-by +100)} ">100")
    (dom/button #js {:onClick (partial jump-to (dec war-count))} ">|")))

(defn set-new-steps [steps] (swap! app-state assoc :new-steps steps))
(defn set-new-setting [key f] (swap! app-state update-in [:new-settings key] f))

(defn- render-num-setting [settings setting-name setting]
  (dom/label nil setting-name
    (dom/input #js {:onChange (fn [this] (set-new-setting setting (constantly (int (.. this -target -value)))))
                    :value (get settings setting)})))

(defn render-inputs [new-settings]
  (dom/div #js {:className "cw-inputs"}
    (dom/button #js {:onClick reinit} "Initialize new war")
    (render-num-setting new-settings "rows" :m)
    (render-num-setting new-settings "cols" :n)
    (render-num-setting new-settings "colors" :c)
    (dom/label nil "Wrap-around?"
      (dom/input #js {:type "checkbox"
                      :onChange (fn [this] (set-new-setting :wrap-around (constantly (.. this -target -value))))
                      :value (get new-settings :wrap-around)}))
    (dom/label nil "Conservative?"
      (dom/input #js {:type "checkbox"
                      :onChange (fn [this] (set-new-setting :election (constantly (if (.. this -target -value) :conservative :random))))
                      :value (= (get new-settings :election) :conservative)}))))

(defn add-steps-and-populate [new-steps i _]
  (do
    (set-new-steps (+ new-steps i))
    (populate (+ new-steps i))))

(defn render-populate [new-steps]
  (dom/div #js {:className "cw-populate"}
    (dom/button #js {:onClick (fn [_] (populate new-steps))} "Calculate more steps")
    (dom/input #js {:onChange (fn [this] (set-new-steps (.. this -target -value)))
                    :value new-steps})
    (dom/button #js {:onClick (partial add-steps-and-populate new-steps   1)} "+1")
    (dom/button #js {:onClick (partial add-steps-and-populate new-steps  10)} "+10")
    (dom/button #js {:onClick (partial add-steps-and-populate new-steps 100)} "+100")))

(om/root
  (fn [data owner]
    (let [{:keys [wars position settings new-settings new-steps time-used]} data
          war (nth wars position)
          {:keys [c m n election]} settings
          war-count (count wars)
          colors (vec (color-strings c))]
      (reify om/IRender
        (render [_]
          (dom/div nil
            (dom/p nil (str "Time used for populating " time-used "ms"))
            (dom/p nil (str "Frame " (inc position) "/" war-count))
            (dom/p nil (str "Election " election))
            (dom/p nil (str "Frequencies " (sort-by (comp - second) (frequencies (map (partial color-of @war) (if war (:cell-ids war) []))))))
            (dom/div #js {:className "cw-grid"} (render-grid colors m n (:grid war)))
            (render-controls war-count)
            (render-inputs new-settings)
            (render-populate new-steps))))))
  app-state
  {:target (. js/document (getElementById "cw-app"))})

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))
