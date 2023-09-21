(import functools [reduce cache])
(import toolz [first partition compose curry juxt])

(defn readlines [filepath]
  (with [f (open filepath "r")]
    (list (map (fn [l] (.strip l)) f))))

;;;;;;;;;;;;;;;;;;;
;;    library    ;;
;;;;;;;;;;;;;;;;;;;

(defn compartments [sack]
  (partition (int (/ (len sack) 2)) sack))

; this is a monoinal operation that we will reduce across rucksack "compartments"
; and across "elf groups"
(defn intersect [a b] (.intersection (set a) (set b)))

; function from () to dict
; (keeps these bindings local)
(defn create_priority_map [] 
  (setv letters (list (map chr (range (ord "a") (+ (ord "z") 1)))))
  (setv LETTERS (list (map (fn [s] (.upper s)) letters)))
  (setv all_letters (+ letters LETTERS))
  (setv prio (range 1 (+ (len all_letters) 1)))
  (dict (zip all_letters prio)))

; cache to save repeated computation
(setv cached_priority_map (cache create_priority_map))

; score fn
(defn get_priority [letter] 
  (.get (cached_priority_map) letter))

(defn part1 [data]
  (setv inner (compose get_priority 
                       first 
                       (curry reduce intersect)
                       compartments))
  (sum (map inner data)))

; groups of 3, in a list
; reduce intersect over each triple, get prio of common element
(defn part2 [rucksacks]
  (setv groups (list (partition 3 rucksacks)))
  (setv inner (compose get_priority first (curry reduce intersect)))
  (sum (map inner groups)))



;;;;;;;;;;;;;;;;
;;    main    ;;
;;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))             ; create a "parser" instance
(.add_argument ps "-f" "--file")       ; expect a --file arg
(setv file (. (.parse_args ps) file))  ; unpack provided file to string

;; (setv file "data/03/test.txt")
;; (setv rucksacks (readlines file))

(setv answers ((juxt part1 part2) (readlines file)))
(print answers)
