(ns cljs-life.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer (timeout <!)])
  (:require-macros [cljs.core.async.macros :refer (go)]))


(defn neighbors
  "Given a set of cells returns a seq of adjacent cells. If n cells share a 
  neighbor then this neighbor will be in the seq n times."
  [cells]
  (for [[x y] cells dx [-1 0 1] dy [-1 0 1]  
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))


(defn in-bounds?
  "Given bounds [width height] and a cell returns true if the cell is inside
  the box from [0,0] to [width height]."
  [[width height] [x y]]
  (and (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn next-state
  "Given the current set of live cells and a bounding box [width height] for 
  the cells' world returns the next state as a set of live cells per the rules 
  of Conway's Game of Life."
  [cells bounds]
  (reduce (fn [alive [neighbor freq]]
            (if (and (in-bounds? bounds neighbor)
                     (or (and (cells neighbor) (<= 2 freq 3))
                         (and (not (cells neighbor)) (= 3 freq))))
              (conj alive neighbor)
              alive))
          (hash-set)
          (frequencies (neighbors cells))))

(defn update-state
  "Given the current state of the world returns the next state."
  [{:keys [cells bounds] :as state}] 
  (assoc state :cells (next-state cells bounds)))

(defn live!
  "Start the simulation loop."
  [app-state]
  (go 
    (while true
      (let [{:keys [playing? speed]} @app-state] 
        (<! (timeout speed))
        (when playing? 
          (swap! app-state update-state))))))

(defn toggle-play-state
  "Given an om cursor and an event object toggles the playing? state"
  [state _]
  (om/transact! state [:playing?] not))

(defn event->cell
  "Calculates which cell on which an event happened"
  [event cell-size]
  (let [target (.-target event)]
    (vector (quot (- (.-pageX event) (.-offsetLeft target)) cell-size) 
            (quot (- (.-pageY event) (.-offsetTop target)) cell-size))))

(defn handle-world-click
  "Handle a user event on the world."
  [state cell-size e]
  (let [cell (event->cell e cell-size)]
    (om/transact! state [:cells] 
                  (fn [cells] 
                    (if (contains? cells cell)
                      (disj cells cell)
                      (conj cells cell))))))
                  
(defn random-cells
  "Returns a random set of live cells within a bounding box."
  [[width height]]
  (set (filter #(> (Math/random) 0.5) 
               (for [x (range 0 width) 
                     y (range 0 width)] 
                 (vector x y)))))

(defn reset-random!
  "Resets the app state cursor with a random set of live cells."
  [app]
  (om/transact! app (fn [{b :bounds :as s}] (assoc s :cells (random-cells b)))))

(defn cell-els
  "Contructs the svg elements to represent the cells."
  [width height cell-size cells]
  (for [cell-x (range 0 width)
        cell-y (range 0 height)]
    (let [alive? (contains? cells [cell-x cell-y])] 
      (dom/rect #js {:x (* cell-size cell-x)
                     :y (* cell-size cell-y)
                     :width cell-size
                     :height cell-size
                     :style #js {:fill (if alive? "#44bbcc" "none")
                                 :stroke "#ccccce"
                                 :stroke-width "0.5"}}))))

(defn world
  "Om component for the world."
  [app owner]
  (reify
    om/IRender
    (render [_]
      (let [{[width height] :bounds cells :cells cell-size :cell-size} app]
        (dom/div nil 
                 (dom/div nil 
                          (dom/button 
                            #js {:onClick (partial toggle-play-state app)}
                            (if (:playing? app) "stop" "play"))
                          (dom/button
                            #js {:onClick #(reset-random! app)}
                            "random"))
                 (apply dom/svg #js {:height (* height cell-size) 
                                     :width (* width cell-size)
                                     :onClick (partial handle-world-click 
                                                       app 
                                                       cell-size)}
                        (cell-els width height cell-size cells)))))))

(defn draw!
  "Kick-off Om with the app-state atom"
  [app-state]
  (om/root world app-state {:target (. js/document (getElementById "app"))}))


(defonce app-state (atom {:cells #{} 
                          :bounds [50 20]
                          :playing? false 
                          :cell-size 20
                          :speed 8}))
(defn main []
  "Entry point for the application."
  (draw! app-state)
  (live! app-state))
