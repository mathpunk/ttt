(ns game.core
  (:require [game.board :refer [game-over opponent empty-board]]
            [game.players :refer [ask-for-move]]
            [game.referee :as officiate]
            [clojure.spec.test.alpha :as stest]
            [game.console :as console]
            [clojure.spec.alpha :as s])
  (:import game.console.ConsolePlayer
           game.players.FormidableComputer))

(defn -main []
  (do
    ;; display splash screen.
    (console/preamble)
    ;; `referee` gets player info.
    (let [match-up (officiate/introduce console/referee {})]
      ;; `referee` begins the match.
      (officiate/begin console/referee match-up))))
