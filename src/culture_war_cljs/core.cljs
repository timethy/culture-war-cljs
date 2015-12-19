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

(defrecord IGridCultureWar [wrap-around m n grid]
  ICultureWar
  (update-color [this v c] (assoc this :grid (update-in grid v (constantly c))))
  (color-of [this v] (get-in grid v))
  (neighbours-of [this v]
    (let [n-8 (set (map (fn [[i j]] (mapv + v [i j])) neighbouring-coords))]
      (if wrap-around
        (map (fn [[i j]] [(mod i m) (mod j n)]) n-8)
        (filter (fn [[i j]] (and (< -1 i m) (< -1 j n))) n-8))))
  (cell-ids [this] (for [i (range m)
                         j (range n)] [i j])))

(defn grid-culture-war [m n wrap-around init-fn]
  (let [grid (mapv #(mapv (partial init-fn %) (range n)) (range m))]
    (map->IGridCultureWar {:m m :n n :wrap-around wrap-around :grid grid})))

(defprotocol IElection
  (majority [this own-color neighbours] "returns a [IMajority object major color]"))

(defn majors-in [neighbours]
  (let [freqs (sort-by (comp - second) (frequencies neighbours))
        [_ max-freq] (first freqs)]
    (map first (take-while (comp (partial = max-freq) second) freqs))))

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
              (update-color old-war id (majority election (color-of war id) (map (partial color-of war) (neighbours-of war id)))))
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
   :wrap false
   :election :random
   :time-used 0})

(defn initial-war [{:keys [m n c wrap]}]
  (grid-culture-war m n wrap (fn [i j] (rand-int c))))

(defn empty-war [{:keys [m n c wrap]}]
  (grid-culture-war m n wrap (constantly 0)))

(defonce app-state (atom {:wars [(empty-war default-settings)]
                          :playing? false
                          :position 0
                          :new-steps 1
                          :settings default-settings
                          :new-settings default-settings}))

; Copied from d3js
(def colors ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"])

(defn- cell-style [c] #js {:backgroundColor c})

(defn render-grid [m n grid]
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
    (fn [{:keys [wars position] :as state}]
      (let [start-time (.getTime (js/Date.))
            war-count (count wars)
            diff (- n war-count)
            unit (println start-time)]
        (if (pos? diff)
          (-> state
            (assoc
              :wars (run-war wars (random-election rand-int) diff)
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
    (dom/input #js {:onChange (fn [this] (set-new-setting setting (constantly (.. this -target -value))))
                    :value (get settings setting)})))

(defn render-inputs [new-settings]
  (dom/div #js {:className "cw-inputs"}
    (dom/button #js {:onClick reinit} "Initialize new war")
    (render-num-setting new-settings "rows" :m)
    (render-num-setting new-settings "cols" :n)
    (render-num-setting new-settings "colors" :c)))

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
          {:keys [m n]} settings
          war-count (count wars)]
      (reify om/IRender
        (render [_]
          (dom/div nil
            (dom/p nil (str "Time used for populating " time-used "ms"))
            (dom/p nil (str "Frame " (inc position) "/" war-count))
            (dom/div #js {:className "cw-grid"} (render-grid m n (:grid war)))
            (render-controls war-count)
            (render-inputs new-settings)
            (render-populate new-steps))))))
  app-state
  {:target (. js/document (getElementById "cw-app"))})

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))
