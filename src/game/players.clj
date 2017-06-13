(ns game.players
  (:require [game.strategy :as strategy]
            [clojure.spec.alpha :as s]))

(defprotocol Player
  (ask-for-move [player board turn] "Given a player, board, and turn, return a board coordinate representing the player's choice."))

(s/fdef ask-for-move
        :args (s/cat :player (s/map-of symbol? string?)
                     :board :game.board/board
                     :turn :game.board/turn))


;; Computer Players
;; ------------------------
(defrecord FormidableComputer [name mark]
  Player
  (ask-for-move [player board turn]
    ;; Selects the best known move. If there is more than one best move, selects one of them randomly.
    (let [good-moves (strategy/best-moves board turn)
          selection first]
      (rand-nth (selection good-moves)))))


;; To create a computer player with a lower skill level, implement a version of Player using a different `selection` function. For example, choose from the cells in the good-moves collection randomly, instead of always picking the first. This will give the computer a chance of selecting a move that is not optimal. This is a simulation of the notion of a 'trembling hand': the computer can find a best move, but their finger can slip while playing it.
