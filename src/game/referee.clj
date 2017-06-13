(ns game.referee
  (:require [clojure.spec.alpha :as s]
            [game.board :refer [play opponent empty-board game-over cell]]
            [game.players :refer [ask-for-move]]))

(defprotocol Referee
  "The Referee is the heart of a tic-tac-toe game, responsible for getting information about players, displaying the state of the board, beginning and ending the game, and running a round."
  (introduce [referee match-up] "Gets information about the players.")
  (nudge [referee player] "Let a player know it's their turn.")
  (announce [referee player move] "Declares the move a player chose.")
  (conclude [referee outcome match-up] "Congratulates the winner, and exits.")
  (display [referee board turn match-up] "Shows a representation of a board."))

(defn round
  "Runs a complete round of play, declaring a conclusion if necessary."
  [referee board turn match-up]
  (if-let [outcome (game-over board)]
    (do
      (display referee board turn match-up)
      (conclude referee outcome match-up))
    (do
      (display referee board turn match-up)
      (nudge referee (turn match-up))
      (let [move (ask-for-move (turn match-up) board turn)]
        (do
          (announce referee (turn match-up) move)
          (round referee (play board turn move) (opponent turn) match-up))))))

(s/fdef round
        :args (s/cat :referee #(satisfies? Referee %)
                     :board :game.board/board
                     :turn :game.board/turn
                     :match-up (s/map-of symbol? string?)))

(defn begin
  "Begins a game with known players."
  [referee match-up]
  (round referee empty-board :p1 match-up))
