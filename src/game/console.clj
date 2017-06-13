(ns game.console
  (:require [game.board :refer [cell]]
            [clojure.set :refer [map-invert]]
            [clojure.spec.alpha :as s]
            [game.referee :refer [Referee]]
            [game.players :refer [Player ask-for-move]])
  (:import game.players.FormidableComputer))

;; Helper
;; ============
(defn- read-non-empty
  "`read-line`, except empty strings are falsey."
  []
  (let [input (read-line)]
    (if-not (empty? input) input)))

;; Human Players Using the Console
;; ================================================
(defrecord ConsolePlayer [name mark]
  Player
  (ask-for-move [player board turn]
    (println (str "Enter the number of the move you want to make, then press <Enter>. >"))
    (let [response (read-line)]
      (if-let [interpretation (get {"1" [0 0]
                                    "2" [0 1]
                                    "3" [0 2]
                                    "4" [1 0]
                                    "5" [1 1]
                                    "6" [1 2]
                                    "7" [2 0]
                                    "8" [2 1]
                                    "9" [2 2]} response)]

        (if (= :blank (cell board interpretation))
          interpretation
          (do
            (println "That square is occupied. Pick a number in one of the boxes on the board.")
            (ask-for-move player board turn)))
        (do
          (println "That's not a move you can make -- only numbers are valid moves.")
          (ask-for-move player board turn))))))


;; Splash Screen
;; ===========================
(defn preamble
  "A little flair."
  []
  (println
   (str
    "TIC|   |   "
    "\n===+===+===\n "
    "  |TAC|   "
    "\n===+===+===\n "
    "  |   |TOE  \n\n")))


;; Refereeing
;; =======================================

;; Introduction helpers
;; ----------------------------------------------------
(defn- default-mark
  [turn]
  (if (= :p1 turn) "X" "O"))

(defn- default-computer-name
  [turn]
  (if (= :p1 turn) "Hal the Computer" "Ava the Robot"))

(defn introduce-player
  "Given :p1 or :p2 and map, requests user input to determine:

  - is the computer or human playing?
  - if a human, what is their name?
  - what mark should be used to represent the player (with the traditional marks as defaults).

  Returns map including the information we'll need about the player,
  keyed by which turn they play on."
  [turn match-up]
  (do
    (print (str "Player " (if (= turn :p1) "One: " "Two: ")))
    (print (str "What is your name? (Or press <Enter> for computer player) > "))
    (flush)
    (if-let [name-response (read-non-empty)]
      (assoc match-up turn
             (ConsolePlayer. name-response (default-mark turn)))
      (assoc match-up turn
             (FormidableComputer. (default-computer-name turn) (default-mark turn))))))

(s/fdef introduce-player
        :args (s/cat :turn :game.board/turn
                     :match-up (s/or :empty {}
                                     :populated (s/map-of symbol? record?))))

(defn mark-player
  "Ask for the `turn`ths player's mark, or default to the traditional X's and O's."
  [turn match-up]
  (print (str "What mark do you want for this player's moves?
               (Press <Enter> for the traditional \"" (default-mark turn) "\"): > "))
  (flush)
  (if-let [mark-response (read-non-empty)]
    (assoc-in match-up [turn :mark] mark-response)
    match-up))

(s/fdef mark-player
        :args (s/cat :turn :game.board/turn
                     :match-up (s/map-of symbol? record?))
        :ret (s/map-of symbol? string?))


;; Display helpers
;; --------------------------------------------------------
(def coord->number
  {[0 0] "1"
   [0 1] "2"
   [0 2] "3"
   [1 0] "4"
   [1 1] "5"
   [1 2] "6"
   [2 0] "7"
   [2 1] "8"
   [2 2] "9"})

(defn glyph
  "Given a board, row index, column index, and access to state, yield either the mark of the appropriate player, or the human-friendly address (an integer, 1-9) for the cell."
  [board turn match-up [r c :as coord]]
  (let [mark (cell board coord)]
    (if (= :blank mark)
      (get coord->number coord)
      (get-in  match-up [mark :mark]))))


;; Referee for a console game
;; ---------------------------------------
(def referee
  (reify Referee
    (introduce [_ match-up]
      (do (println "Let's play some tic-tac-toe!")
          (let [match-up (->> {}
                              (introduce-player :p1)
                              (mark-player :p1)
                              (introduce-player :p2)
                              (mark-player :p2))]
            (println "\nReady... set... go!\n")
            match-up)))
    (announce [_ player move]
      (println (str (:name player)
                    " plays "
                    (:mark player)
                    " at "
                    (get coord->number move)
                    ":")))
    (nudge [_ player]
      (println (str "It's " (:name player) "'s turn.\n")))
    (conclude [_ outcome match-up]
      (when (= outcome :tie)
        (println "It's a draw!")
        (System/exit 0))
      (println (str (get-in match-up [outcome :name]) " wins!"))
      (System/exit 0))
    (display [_ board turn match-up]
      (let [read-glyph (partial glyph board turn match-up)]
        (println (str
                  " "
                  (read-glyph [0 0]) " | " (read-glyph [0 1]) " | " (read-glyph [0 2])
                  "\n===+===+===\n "
                  (read-glyph [1 0]) " | " (read-glyph [1 1]) " | " (read-glyph [1 2])
                  "\n===+===+===\n "
                  (read-glyph [2 0]) " | " (read-glyph [2 1]) " | " (read-glyph [2 2])
                  "\n"))))))
