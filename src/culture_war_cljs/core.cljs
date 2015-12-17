(ns culture-war-cljs.core
  (:require [clojure.browser.repl :as repl]
            [quiescent.core :as q]
            [quiescent.dom :as d]
            [cljs.core.async :refer [chan close! >! <!]]
            [monet.canvas :as mc])
  (:require-macros
    [quiescent.core :as q]
    [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

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

;; Rendering control code here

; Copied from d3js
(def colors ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"])

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

(defn cartesian [col1 col2]
  (mapcat #(map (partial vector %) col2) col1))

(defn eight-neighbours [wrap]
  (fn [lengths coords]
    (disj
      (->>
        (map (fn [length coord]
               (map #(if wrap
                       (mod (+ coord %) length)
                       (min (max (+ coord %) 0) (dec length))) [-1 0 1]))
          lengths coords)
        (apply cartesian)
        set)
      coords)))

(defn maxima [colors]
  (let [groups (reduce (fn [prev c] (assoc prev c (inc (get prev c 0)))) {} colors)
        max-count (apply max (vals groups))]
    (->> groups
      (filter (comp #(= max-count %) second))
      keys
      vec)))

(defn new-color [colors random-fn]
  (if (= 1 (count colors))
    (first colors)
    (nth colors (random-fn (count colors)))))

(defn initial-war [c m n]
  (grid-culture-war m n false (fn [i j] (rand-int c))))

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))

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

(defn app-loop [stop-ch stopped-ch war {:keys [wrap timestep]}]
  (let [dom (.getElementById js/document "cw-canvas")
        ctx (mc/get-context dom "2d")
        grid (:grid war)
        election (random-election rand-int)]
    (println (keys war))
    (. ctx (clearRect 0 0 (.-width dom) (.-height dom)))
    (draw-grid ctx grid nil)
    (go-loop [w war]
      (draw-grid ctx (:grid w) nil)
      (let [ctrl (alt!
                   stop-ch :stop
                   (timeout timestep) :continue)]
        (case ctrl
          :stop (>! stopped-ch :stopped)
          :continue (recur (update-war w election)))))))

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
            {:keys [colors m n timestep]} settings]
        (cond
          (vector? ctrl)  (do (println ctrl) (let [[key value] ctrl] (recur (assoc settings key value :running false))))
          (= ctrl :start) (do
                            (app-loop stop-channel stopped-channel (initial-war colors m n) settings)
                            (recur (assoc settings :running true))))))))

(enable-console-print!)
(control-loop)
