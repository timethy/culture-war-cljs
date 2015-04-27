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

(defn initial-grid [c m n]
  (apply array
    (for [i (range m)]
      (apply array
        (for [j (range n)]
          (rand-int c))))))

(defn step
  "We guarantee to always call the random-fn in the same order, so randomness can be pregenerated."
  [[grid new-grid] neighbour-fn random-fn]
  (let [lengths [(count grid) (count (first grid))]]
    (doseq [i (range (first lengths))
            j (range (second lengths))]
      (let [neighbours (neighbour-fn lengths [i j])
            neighbour-colors (map (fn [[x y]] (aget (aget grid x) y)) neighbours)
            max-colors (maxima neighbour-colors)]
        (aset (aget new-grid i)
          j (new-color max-colors random-fn))))
    [new-grid grid]))

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))

(def width 16)
(def height 16)

(defn draw-grid [ctx grid old-grid]
  (let [lengths [(count grid) (count (first grid))]
        w (dec width)
        h (dec height)]
    (doseq [i (range (first lengths))
            j (range (second lengths))]
      (when (or (nil? old-grid) (not= (aget (aget grid i) j) (aget (aget old-grid i) j)))
        (set! (.-fillStyle ctx) (colors (aget (aget grid i) j)))
        (. ctx (fillRect (* j width) (* i height) w h))))))

(defn copy [grid]
  (apply array (map (partial apply array) grid)))

(defn app-loop [stop-ch stopped-ch grid {:keys [wrap timestep]}]
  (let [dom (.getElementById js/document "cw-canvas")
        ctx (mc/get-context dom "2d")]
    (. ctx (clearRect 0 0 (.-width dom) (.-height dom)))
    (draw-grid ctx grid nil)
    (go-loop [g [(copy grid) (copy grid)]]
      (draw-grid ctx (first g) (second g))
      (let [ctrl (alt!
                   stop-ch :stop
                   (timeout timestep) :continue)]
        (case ctrl
          :stop (>! stopped-ch :stopped)
          :continue (recur (step g (eight-neighbours wrap) rand-int)))))))

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
                            (app-loop stop-channel stopped-channel (initial-grid colors m n) settings)
                            (recur (assoc settings :running true))))))))

(enable-console-print!)
(control-loop)
