(ns culture-war-cljs.core
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan close! >! <!]]
            [monet.canvas :as mc]
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
   :time-between-frame 100})

(defn initial-war [{:keys [m n c wrap]}]
  (grid-culture-war m n wrap (fn [i j] (rand-int c))))

(defn empty-war [{:keys [m n c wrap]}]
  (grid-culture-war m n wrap (constantly 0)))

(defonce app-state (atom {:wars [(empty-war default-settings)]
                          :play false
                          :position 0
                          :settings default-settings
                          :new-settings default-settings}))

; Copied from d3js
(def colors ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"])

(defn- cell-style [c]
  #js {:backgroundColor c})

(defn render-grid [m n grid]
  (for [i (range m)]
    (dom/div #js {:className "cw-row"}
      (for [j (range n)]
        (dom/span #js {:className "cw-cell"
                       :style (cell-style (colors (get-in grid [i j])))}
          ".")))))

(def play-chan (chan))

(defn jump-to [n] (swap! app-state update-in [:position] (constantly n)))
(defn jump-by [n] (swap! app-state (fn [{:keys [position wars] :as args}]
                                     (assoc args :position (-> position (+ n) (max 0) (min (dec (count wars))))))))

(defn reinit []
  (swap! app-state (fn [{:keys [new-settings] :as args}]
                     (assoc args
                       :wars [(initial-war new-settings)]
                       :settings new-settings))))

(defn populate [n]
  (swap! app-state update-in [:wars]
    (fn [wars]
      (let [war-count (count wars)
            diff (- n war-count)]
        (if (pos? diff)
          (run-war wars (random-election rand-int) diff)
          wars)))))

(defn render-controls [war-count]
  [(dom/button #js {:onClick (partial jump-to 0)} "|<")
   (dom/button #js {:onClick (partial jump-by -100)} "<100")
   (dom/button #js {:onClick (partial jump-by -10)} "<10")
   (dom/button #js {:onClick (partial jump-by -1)} "<")
   (dom/button nil "Play")
   (dom/button #js {:onClick (partial jump-by +1)} ">")
   (dom/button #js {:onClick (partial jump-by +10)} ">10")
   (dom/button #js {:onClick (partial jump-by +100)} ">100")
   (dom/button #js {:onClick (partial jump-to (dec war-count))} ">|")])

(defn set-new-setting [key f]
  (swap! app-state update-in [:new-settings key] f))

(defn render-inputs [new-settings]
  [(dom/button #js {:onClick (fn [_] (do (reinit) (populate 100)))} "New war")
   (dom/label nil "Test"
     (dom/input #js {:onChange (fn [this] (set-new-setting :m (constantly (.-value this))))}))])

(om/root
  (fn [data owner]
    (let [{:keys [wars position settings new-settings]} data
          war (nth wars position)
          {:keys [m n time-between-frame]} settings
          war-count (count wars)]
      (reify om/IRender
        (render [_]
          (dom/div nil
            (dom/p nil (str "Time betweeen frames " time-between-frame "ms"))
            (dom/p nil (str "Frame " position "/" (dec war-count)))
            (dom/div #js {:className "cw-grid"} (render-grid m n (:grid war)))
            (dom/div #js {:className "cw-controls"}
              (render-controls war-count))
            (dom/div #js {:className "cw-inputs"}
              (render-inputs new-settings)))))))
  app-state
  {:target (. js/document (getElementById "cw-app"))})

(comment
(q/defcomponent CultureWarControls
  [settings control-ch stop-ch]
  (apply d/div {}
    (if (:running settings)
      (d/button {:onClick #(go (>! stop-ch :stop))} "Stop")
      (d/button {:onClick #(go (>! control-ch :start))} "Start"))
    (for [setting [:colors :m :n :timestep :wrap]
          elem [(d/span {} (name setting))
                (d/input {:value (settings setting)
                          :type "text"
                          :onChange
                          (fn [evt]
                            (let [v (.-value (.-target evt))]
                              (go (>! control-ch [setting v]))))})]]
      elem)))

(def width 16)
(def height 16)

(defn draw-grid [ctx grid old-grid]
  (let [lengths [(count grid) (count (first grid))]
        w (dec width)
        h (dec height)]
    (doseq [i (range (first lengths))
            j (range (second lengths))]
      (when (or (nil? old-grid) (not= (get (get grid i) j) (get (get old-grid i) j)))
        (set! (.-fillStyle ctx) (colors (get (get grid i) j)))
        (. ctx (fillRect (* j width) (* i height) w h))))))

(defn copy [grid]
  (apply array (map (partial apply array) grid)))

(defn app-loop [stop-ch stopped-ch war {:keys [timestep]}]
  (let [dom (.getElementById js/document "cw-canvas")
        ctx (mc/get-context dom "2d")
        grid (:grid war)
        election (random-election rand-int)]
    (. ctx (clearRect 0 0 (.-width dom) (.-height dom)))
    (draw-grid ctx grid nil)
    (go-loop [w war
              n 0]
;      (println n)
      (draw-grid ctx (:grid w) nil)
      (let [ctrl (alt!
                   stop-ch :stop
                   (timeout timestep) :continue)]
        (case ctrl
          :stop (>! stopped-ch :stopped)
          :continue (recur (update-war w election) (inc n)))))))

(defn control-loop []
  (let [control-channel (chan)
        stop-channel (chan)
        stopped-channel (chan)
        control-div (.getElementById js/document "cw-controls")]
    (go-loop [settings {:colors 10, :m 10, :n 10, :wrap false, :timestep 250, :running false}]
      (q/render (CultureWarControls settings control-channel stop-channel) control-div)
      (if (:running settings)
        (<! stopped-channel))
      (q/render (CultureWarControls (assoc settings :running false) control-channel stop-channel) control-div)
      (let [ctrl (<! control-channel)
            {:keys [colors m n timestep wrap]} settings]
        (cond ; ctrl: vector is setting a setting, number is jumping to a step, :simulate is resume simulation from current step
          (vector? ctrl) (do (println ctrl) (let [[key value] ctrl] (recur (assoc settings key value :running false))))
          (= ctrl :simulate) (do
                               (app-loop stop-channel stopped-channel
                                 (grid-culture-war m n wrap (fn [i j] (rand-int colors)))
                                 settings)
                               (recur (assoc settings :running true)))))))))

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))
