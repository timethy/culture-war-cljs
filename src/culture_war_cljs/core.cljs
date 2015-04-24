(ns culture-war-cljs.core
  (:require [clojure.browser.repl :as repl]
            [quiescent.core :as q]
            [quiescent.dom :as d]
            [cljs.core.async :refer [chan close!]])
  (:require-macros
    [cljs.core.async.macros :as m :refer [go]]))

;; (repl/connect "http://localhost:9000/repl")

; Copied from d3js
(def colors ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"])

(q/defcomponent CultureWarDiv
  [grid]
  (apply d/div
    (for [row grid]
      (apply d/div {:className "cw-row"}
        (for [elem row]
          (d/div {:className "cw-elem"
                  :style {:backgroundColor (colors elem)}}))))))

(defn cartesian [col1 col2]
  (mapcat #(map (partial vector %) col2) col1))
;  (for [c1 col1]
;    (for [c2 col2]
;      [c1 c2])))

(defn eight-neighbours [lengths coords]
  (disj
    (->>
      (map (fn [length coord] (map #(mod (+ coord %) length) [-1 0 1])) lengths coords)
      (apply cartesian)
      set)
    coords))

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
  (vec (for [i (range m)]
         (vec (for [j (range n)]
                (rand-int c))))))

(defn step
  "We guarantee to always call the random-fn in the same order, so randomness can be pregenerated."
  [neighbour-fn random-fn grid]
  (let [lengths [(count grid) (count (first grid))]]
    (println lengths)
    (vec (for [i (range (first lengths))]
           (vec (for [j (range (second lengths))]
                  (let [neighbours (neighbour-fn lengths [i j])
                        neighbour-colors (map (partial get-in grid) neighbours)
                        max-colors (maxima neighbour-colors)]
                    (new-color max-colors random-fn))))))))

(defn timeout [ms] (let [c (chan)] (js/setTimeout (fn [] (close! c)) ms) c))

(defn app-loop [timestep]
  (go
    (loop [grid (initial-grid 2 100 100)]
      (q/render (CultureWarDiv grid)
        (.getElementById js/document "cw-grid"))
      (let [new-grid (step eight-neighbours rand-int grid)]
        (<! (timeout timestep))
        (recur new-grid)))))

(enable-console-print!)
(app-loop 40)

(println "Hello world!")
