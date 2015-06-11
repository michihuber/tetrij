(ns tetrij.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [reagent.core :as reagent]
            [cljs.core.async :refer [chan dropping-buffer put! >! <! alts! timeout close!]]))

(def tiles {:o [[0 0]  [0 1]  [1 1]  [1 0]]
            :i [[-2 0] [-1 0] [0 0]  [1 0]]
            :t [[-1 0] [0 0]  [1 0]  [0 -1]]
            :l [[-1 0] [0 0]  [1 0]  [1 -1]]
            :j [[1 0]  [0 0]  [-1 0] [-1 -1]]
            :s [[-1 0] [0 0]  [0 1]  [1 1]]
            :z [[-1 1] [0 1]  [0 0]  [1 0]]})

(def initial-tile-position [5 0])

(defn empty-row [width]
  (vec (repeat width nil)))

(defn empty-board
  ([width height] (vec (repeat height (empty-row width))))
  ([] (empty-board 11 22)))

(defn board-width [board]
  (-> board first count))

(defn random-tile-type []
  (-> tiles keys rand-nth))

(defn tile-rotated-cw [tile]
  (if (= tile (tiles :o)) tile (mapv (fn [[x y]] [(- y) x]) tile)))

(defn tile-rotated-ccw [tile]
  (if (= tile (tiles :o)) tile (mapv (fn [[x y]] [y (- x)]) tile)))

(defn absolute-coords [tile [x y]]
  (map (fn [[dx dy]] [(+ dx x) (+ dy y)]) tile))

(defn insert-at [board tile tile-type [x y :as position]]
  (reduce (fn [b [x y]] (if ((not neg?) y) (assoc-in b [y x] tile-type) b))
            board
            (absolute-coords tile position)))

(defn within-bounds? [board tile position]
  (let [board-height (count board)
        tile-coords (absolute-coords tile position)
        x-coords (map #(nth % 0) tile-coords)
        y-coords (map #(nth % 1) tile-coords)
        ok-x? (every? #(and ((not neg?) %) (< % (board-width board))) x-coords)
        ok-y? (every? #(< % board-height) y-coords)]
    (and ok-x? ok-y?)))

(defn collides-with-board? [board tile position]
  (let [tile-coords (absolute-coords tile position)]
    (not-every? (fn [[x y]] (nil? (get-in board [y x]))) tile-coords)))

(defn collides? [board tile position]
  (or (collides-with-board? board tile position)
      (not (within-bounds? board tile position))))

(defn rotate [{board :board old-tile :tile position :tile-position} rotation-fn]
  (let [new-tile (rotation-fn old-tile)]
    (if (collides? board new-tile position)
      {:tile old-tile}
      {:tile (rotation-fn old-tile)})))

(defn rotate-cw [game] (rotate game tile-rotated-cw))
(defn rotate-ccw [game] (rotate game tile-rotated-ccw))

(defn move-x [{[x y :as old-pos] :tile-position tile :tile board :board} move-fn]
  (let [new-pos [(move-fn x) y]]
    (if (collides? board tile new-pos) {:tile-position old-pos}
                                       {:tile-position new-pos})))

(defn move-left [game] (move-x game dec))
(defn move-right [game] (move-x game inc))

(defn move-down [{[x y] :tile-position :keys [board tile tile-type next-tile-type]}]
  (let [new-pos [x (inc y)]]
    (if (collides? board tile new-pos)
      {:board          (insert-at board tile tile-type [x y])
       :tile           (next-tile-type tiles)
       :tile-type      next-tile-type
       :tile-position  initial-tile-position
       :next-tile-type (random-tile-type)}
      {:tile-position new-pos})))

(defn drop-tile [game]
  (let [game-update (move-down game)]
    (if (some? (:board game-update))
      game-update
      (drop-tile (merge game game-update)))))

(defn full-row? [row]
  (every? some? row))

(defn cleared-board [board]
  (mapv #(if (full-row? %) (empty-row (board-width board)) %) board))

(defn compacted-board [board]
  (let [full-row-count (->> board (filter full-row?) count)
        width (board-width board)]
    (->> board
         (remove full-row?)
         (concat (vec (repeat full-row-count (empty-row width))))
         vec)))

(def game
  (let [first-tile-type (random-tile-type)]
    (reagent/atom {:lines 0 :level 0 :score 0
                   :board (empty-board)
                   :tile (first-tile-type tiles)
                   :tile-type first-tile-type
                   :next-tile-type (random-tile-type)
                   :tile-position initial-tile-position})))

;; event handling
(defn keycode->event [code]
  (case code
    37 :move-left  ;; left arrow
    39 :move-right ;; right arrow
    69 :rotate-ccw ;; e
    82 :rotate-cw  ;; r
    38 :rotate-cw  ;; up arrow
    40 :move-down  ;; down arrow
    32 :drop-tile  ;; space
    80 :pause
    nil))

(def keyevent->event (comp (map #(.-keyCode %))
                           (map keycode->event)
                           (remove nil?)))

(def ticker (atom nil))
(def user-chan (chan 1 keyevent->event))
(def event-chan (chan (dropping-buffer 1)))
(def pause-chan (chan))

(defn exp [x n] (reduce * (repeat n x)))

(defn set-ticker [ms]
  (if-let [ticker @ticker] (js/window.clearInterval ticker))
  (reset! ticker (js/window.setInterval #(put! event-chan :move-down) ms)))

(defn set-level-speed [level]
  (let [ms (/ 1000 (exp 1.414 level))]
    (set-ticker ms)))

(set-level-speed 0)

(set! (.-onkeydown js/document) #(put! user-chan %))
(go-loop []
  (let [event (<! user-chan)]
    (if (= :pause event)
      (>! pause-chan event)
      (>! event-chan event))
  (recur)))

(def base-score [40 100 300 1200])
(defn lines-for-next-level [current-level]
  ;; we add 2 (not 1) because range is exclusive (and we want the *next* level)
  (* 5 (reduce + (range (+ current-level 2)))))

(go-loop []
  (let [[event event-channel] (alts! [event-chan pause-chan])]
    (case event
      :pause      (do
                    (swap! game assoc :paused true)
                    (<! pause-chan)
                    (swap! game assoc :paused false))
      :drop-tile  (swap! game merge (drop-tile @game))
      :move-left  (swap! game merge (move-left @game))
      :move-right (swap! game merge (move-right @game))
      :move-down  (swap! game merge (move-down @game))
      :rotate-cw  (swap! game merge (rotate-cw @game))
      :rotate-ccw (swap! game merge (rotate-ccw @game)))
    (let [{:keys [board level]} @game
          full-row-count (count (filter full-row? board))]
      (when-not (zero? full-row-count)
        (swap! game update-in [:lines] + full-row-count)
        (swap! game update-in [:score] + (* (inc level)
                                            (base-score (dec full-row-count))))

        (if (> (:lines @game) (lines-for-next-level level))
          (swap! game update-in [:level] inc))

        (swap! game update-in [:board] cleared-board)
        (<! (timeout 125))
        (swap! game assoc :board board)
        (<! (timeout 125))
        (swap! game update-in [:board] cleared-board)
        (<! (timeout 125))
        (swap! game assoc :board (compacted-board board))

        (set-level-speed (:level @game))))
    (let [{:keys [board tile tile-position]} @game]
      (if-not (collides? board tile tile-position)
        (recur)
        (do
          (close! event-chan)
          (swap! game assoc :game-over true))))))

;; rendering
(defn render-to [component id]
  (reagent/render-component [component] (.getElementById js/document id)))

(defn rendered-board [board]
  (map-indexed (fn [i row]
         [:div.row {:key i} (map-indexed #(vec [:div.cell {:class %2 :key (str "c" i "-" %1)} ""]) row)])
       board))

(defn game-view []
  (let [{:keys [board tile tile-type tile-position paused game-over]} @game
        board-with-tile (insert-at board tile tile-type tile-position)]
  [:div#game
    (if game-over [:div#info "Game Over"])
    (if paused [:div#info "Paused"])
    (if-not paused
      (rendered-board board-with-tile)
      (rendered-board (empty-board)))]))

(defn next-tile-view []
  (let [empty-view (empty-board 5 5)
        ntt (:next-tile-type @game)
        nt (ntt tiles)
        view (insert-at empty-view nt ntt [2 1])]
    [:div#next-tile (rendered-board view)]))

(defn stats-view []
  (let [{:keys [level lines score]} @game]
    [:div
     [:div.stats level]
     [:div.stats lines]
     [:div.stats score]]))

(render-to game-view      "main")
(render-to stats-view     "sidebar-stats")
(render-to next-tile-view "sidebar-next-tile")
