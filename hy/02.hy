;;;;;;;;;;;;;;;;;;;
;;    library    ;;
;;;;;;;;;;;;;;;;;;;

(import toolz [first second juxt])

(defn readlines [filepath]
  (with [f (open filepath "r")]
    (list (map (fn [l] (tuple (.split (.strip l)))) f))))

; --- scoring the games ---

(defn score_shape [sh]
  (match sh "rock" 1
            "paper" 2
            "scissors" 3 ))

(defn score_outcome [outcome]
  (match outcome "win" 6
                 "lose" 0
                 "draw" 3))


; --- part 1 ---

(defn decode_move [k]
  (cond 
    (in k #{"A" "X"}) "rock"
    (in k #{"B" "Y"}) "paper"
    (in k #{"C" "Z"}) "scissors"))

(defn tic-tac-toe [theirs mine]
  (setv wins #{#("rock" "paper")
               #("paper" "scissors") 
               #("scissors" "rock")})
  (setv losses #{#("scissors" "paper") 
                 #("rock" "scissors") 
                 #("paper" "rock")})
  (setv draws #{#("scissors" "scissors") 
                #("rock" "rock") 
                #("paper" "paper")})
  (cond (in #(theirs mine) wins) "win"
        (in #(theirs mine) losses) "lose"
        (in #(theirs mine) draws) "draw"))

(defn score-round [moves]
  (setv [theirs mine] (map decode_move moves))
  (+ (score_outcome (tic-tac-toe theirs mine))
     (score_shape mine)))

(defn part1 [guide]
  (sum (map score-round guide)))


; --- part 2 ---

(defn find_move [theirs outcome]
  (setv wins {"rock" "paper"
              "paper" "scissors"
              "scissors" "rock"})
  (setv losses {"paper" "rock" 
                "scissors" "paper"
                "rock" "scissors"})
  (setv draws {"paper" "paper" 
               "scissors" "scissors"
               "rock" "rock"})
  (match outcome 
    "win" (.get wins theirs)
    "lose" (.get losses theirs)
    "draw" (.get draws theirs)))

(defn decode_outcome [outcome]
  (match outcome "X" "lose"
                 "Y" "draw"
                 "Z" "win"))

(defn pt2-round [round-guide]
  (setv theirs (decode_move (first round-guide)))
  (setv outcome (decode_outcome (second round-guide)))
  (setv mine (find_move theirs outcome))
  (+ (score_outcome outcome) (score_shape mine)))

(defn part2 [guide]
  (sum (map pt2-round guide)))


;;;;;;;;;;;;;;;;
;;    main    ;;
;;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))             ; create a "parser" instance
(.add_argument ps "-f" "--file")       ; expect a --file arg
(setv file (. (.parse_args ps) file))  ; unpack provided file to string

(setv answers ((juxt part1 part2) (readlines file)))
(print answers)

