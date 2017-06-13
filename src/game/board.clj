(ns game.board
  (:require [clojure.spec.alpha :as s]))

(s/def ::coord (s/coll-of (s/int-in 0 3) :kind seqable? :count 2))

(s/def ::mark #{:p1 :p2 :blank}) ;; C.f. symbol appearing on the board to a user, which I call 'glyphs' to avoid confusion with Clojure symbols.

(s/def ::turn #{:p1 :p2}) ;; At one time was called 'player,' which caused confusion with reifications of the Player protocol

(s/def ::triple (s/map-of ::coord ::mark :count 3 :into {}))

(s/def ::board (s/map-of ::coord ::mark :count 9 :into {}))

(s/def ::in-range #{0 1 2}) ;; For checking row- and col-indices.

(defn opponent
  "Yields the turn of the player opposing this turn."
  [turn]
  (if (= turn :p1) :p2 :p1))

(def empty-board
  {[0 0] :blank
   [0 1] :blank
   [0 2] :blank
   [1 0] :blank
   [1 1] :blank
   [1 2] :blank
   [2 0] :blank
   [2 1] :blank
   [2 2] :blank})

(defn cell
  "Return the marker of the cell in (row, col) of the given board. The coordinates may or may not be wrapped in a seq."
  ([board coord]
   (cell board (first coord) (second coord)))
  ([board row col]
  (get board [row col])))

(s/fdef cell
        :args (s/or :unpacked-coordinates (s/cat :board ::board
                                                 :row ::in-range
                                                 :col ::in-range)
                    :packed-coordinates (s/cat :board ::board
                                               :coord ::coord))
        :ret ::mark) 

(defn cells
  "Return the markers of all cells, in lexicographic order of coordinates."
  [board]
  (vals (sort board)))

(s/fdef cells
        :args (s/cat :board ::board)
        :ret (s/coll-of ::mark :count 9))

(defn row
  "Return the nth row of a board."
  [board n]
  (into {} (filter (fn [[k v]] (= (first k) n)) board)))

(s/fdef row
        :args (s/cat :board ::board :row-index ::in-range)
        :ret ::triple)

(defn col
  "Return the nth column of a board."
  [board n]
  (into {} (filter (fn [[k v]] (= (second k) n)) board)))

(s/fdef col
        :args (s/cat :board ::board :col-index #{0 1 2})
        :ret ::triple)

(defn rows
  "Return all rows of a board"
  [board]
  (map #(row board %) [0 1 2]))

(s/fdef rows
         :args (s/cat :board ::board)
         :ret seqable?)

(defn cols
  "Return all columns of a board"
  [board]
  (map #(col board %) [0 1 2]))

(s/fdef cols
        :args (s/cat :board ::board)
        :ret seqable?)

(defn neg-diag
  "Return the diagonal with negative slope of a board."
  [board]
  (into {} (filter (fn [[k v]] (= (first k) (second k))) board)))

(s/fdef neg-diag
        :args (s/cat :board ::board)
        :ret ::triple)

(defn pos-diag
  "Return the diagonal with positive slope of a board."
  [board]
  (into {} (filter (fn [[k v]] (= 2 (+ (first k) (second k)))) board)))

(s/fdef pos-diag
        :args (s/cat :board ::board)
        :ret ::triple)

(defn diags [board]
  (list (neg-diag board) (pos-diag board)))

(s/fdef diags
        :args (s/cat :board ::board )
        :ret (s/coll-of ::triple :count 2))

(defn candidates
  "Returns a sequence of the coordinates of a board marked :blank.

  Note: this seems more complicated than it need be."
  [board]
  (keys (into {} (filter (fn [[k v]] (= :blank v)) board))))

(defn corners [board]
  (select-keys board [[0 0] [0 2] [2 0] [2 2]]))

(defn opposite-corner [coord]
  (map (fn [c] (case c
                 0 2
                 2 0)) coord))

(defn unplayed?
  "True if unmarked by any player."
  [mark]
  (= mark :blank))

(defn winning-triple
  "Given a sequence of positions of length three, return :p1, :p2, or false."
  [triple]
  (let [marks (vals triple)]
    (if (and (apply = marks)
             (not-any? unplayed? marks))
      (first marks)
      false)))

(s/fdef winning-triple
        :args (s/cat :triple ::triple )
        :ret (s/or :winner ::turn :no-winner false?))

(defn game-over
  "Given a board, return the winner, :tie, or falsey as appropriate."
  [board]
  (or (let [triples (concat (rows board) (cols board) (diags board))]
        (some winning-triple triples))
      (if (not-any? unplayed? (cells board)) :tie)))

(s/fdef game-over
        :args (s/cat :board ::board)
        :ret (s/or :winner ::turn :tie #{:tie} :ongoing false?))

(defn play
  "Given a board, player, and address, return the board resulting in that play. If the play is impossible because the position is filled, the current implementation returns the board unchanged. (That smells funny to me but hasn't caused problems so far.)"
  [board turn [r c :as coord]]
  (if-not (= :blank (cell board r c))
    board
    (assoc board coord turn)))

(s/fdef play
        :args (s/cat :board ::board :turn ::turn :coord ::coord)
        :ret (s/cat :board ::board))
