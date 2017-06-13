(ns game.strategy
  (:require [game.board :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]))

;; Optimal Tic-Tac-Toe Strategy
;; ========================================
;; Relying on the Wikipedia description of Newell and Simon's 1972 tic-tac-toe program.
;; Optimal play is to choose the earliest possible of these moves, in this order. See
;; http://onlinelibrary.wiley.com/doi/10.1207/s15516709cog1704_3/epdf for more detail.


;; Play-finding Functions
;; ==============================
;; Following are functions which return sequences of candidates for the turn's player to play (and some helpers). These individual strategies will be combined into a function that takes them, in this order, to generate a sequence of the best plays.

;; Win
;; --------------
;; If the player can win, win.

(defn winning-play?
  "Given a board, the turn, and an address to mark, return true if the turn's player would win."
  [board turn [r c :as coord]]
  (= turn (game-over (play board turn coord))))

(s/fdef winning-play?
        :args (s/cat :board :game.board/board
                     :turn  :game.board/turn
                     :coord :game.board/coord)
        :ret boolean?)

(defn winning-plays
  "Given a board and the turn, return a seq of coordinates representing winning plays."
  [board turn]
  (filter (partial winning-play? board turn)
          (cartesian-product [0 1 2] [0 1 2])))

(s/fdef winning-plays
        :args (s/cat :board :game.board/board
                     :turn  :game.board/turn)
        :ret (s/* :game.board/coord))

;; Block
;; ----------------
;; If the opponent can win, block the play. (If the opponent has two non-equal ways to win, we're sunk.)
(defn blocking-plays
  "Given a board and the turn, return a seq of coordinates representing blocking plays."
  [board turn]
    (winning-plays board (opponent turn)))

(s/fdef blocking-plays
        :args (s/cat :board  :game.board/board
                     :turn :game.board/turn)
        :ret (s/* :game.board/coord))

;; Fork
;; ----------------
;; Create an opportunity where the turn's player has two threats to win (two non-blocked lines of 2). Put another way, play in such a manner that, if the turn's player could move twice, they would have two ways to win.
(defn forking-play?
  [board turn [r c :as coord]]
    (let [subjunctive-board (play board turn coord)]
      (> (count (winning-plays subjunctive-board turn)) 1)))

(defn forking-plays
  "A play is a forking play if, when played, the turn's turn has two (more?) ways to win."
  [board turn]
  (let [candidates (candidates board)]
    (filter #(forking-play? board turn %) candidates)))

(s/fdef forking-plays
        :args (s/cat :board :game.board/board
                     :turn  :game.board/turn)
        :ret (s/* :game.board/coord))

;; Blocking an opponent's fork
;; -------------------------------------
;; Option 1: Play such that the resulting board threatens a win, and that threatened winning position is not a fork for my opponent.
;; Option 2: Play what would be my opponent's forking play.
(defn fork-blocking-plays-1
    [board turn]
  (let [candidates (candidates board)]
    (filter (fn [candidate]
              (let [subjunctive-board (play board turn candidate)
                    threatened-wins (winning-plays subjunctive-board turn)]
                (seq (remove #(forking-play? subjunctive-board (opponent turn) %) threatened-wins) )))
            candidates)))

(defn fork-blocking-plays-2
  [board turn]
  (forking-plays board (opponent turn)))

;; Center
;; ------------------
;; A turn's player marks the center. (If it is the first move of the game, playing on a corner gives "O" more opportunities to make a mistake and may therefore be the better choice; however, it makes no difference between perfect players.)
(defn center-play
  [board turn]
  (case (cell board 1 1)
    :blank (list [1 1])
    nil))

;; Opposite corner
;; ------------------
;; If the opponent is in the corner, the turn's player plays the opposite corner.
(defn opposite-corner-plays
  [board turn]
  (let [opponents-corners (keys (into {} (filter (fn [[k v]] (= v (opponent turn))) (corners board))))]
    (->> opponents-corners
         (map (fn [corner]
                (if (= (cell board (opposite-corner corner))
                       :blank)
                  (opposite-corner corner)
                  nil)))
         (filter identity))))

;; Empty corner
;; ------------------
;; The turn's player plays in a corner square.
(defn empty-corner-plays
  ([board & turn]
   (-> board corners candidates)))

;; Empty side
;; ------------------
;; The turn's player plays in a middle square on any of the 4 sides.
(defn empty-side-plays
  [board & turn]
  (candidates (select-keys board [[0 1] [1 0] [1 2] [2 1]])))

(defn just-play-something
  "To make sure the computer doesn't just sit and pout if it's going to lose, the last collection of plays is the set of all empty spaces."
  [board & turn]
  (candidates board))


;; Composition of plays-finding functions
;; ============================================
;; Combining the *-plays functions above in order yields a collection of collections of moves. The first collection of moves is the optimal one. The remaining collections are, at a guess, in approximate order of their goodness. See `players.clj` for an example of their use for optimal play, and for suggestions on how to use them for computers of lesser skill.
(defn best-moves
  "Takes a board and turn. Applies the play strategy functions to the board/turn pair, and removes any empty strategies. The result is a sequence of sequences of coordinates."
  [board turn]
  (remove empty?
          ((juxt winning-plays
                 blocking-plays
                 forking-plays
                 fork-blocking-plays-1
                 fork-blocking-plays-2
                 center-play
                 opposite-corner-plays
                 empty-corner-plays
                 empty-side-plays
                 just-play-something) board turn)))
