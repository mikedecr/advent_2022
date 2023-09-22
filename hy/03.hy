(import functools [reduce cache])
(import toolz [first last partition compose curry juxt])

(defn readlines [filepath]
  (with [f (open filepath "r")]
    (list (map (fn [l] (.strip l)) f))))

;;;;;;;;;;;;;;;;;;;
;;    library    ;;
;;;;;;;;;;;;;;;;;;;

; our core here is very small

; toolz.partition splits at an index.
; we invert that to split into n groups
(defn partition_into [n_grps x]
  (setv index (/ (len x) n_grps))
  (tuple (partition (int index) x)))

(defn is_singleton [x] (= (len x) 1))

; this looks trivial at first but will actually unlock a lot of power for us
(defn intersect [a b] (.intersection (set a) (set b)))

; if `intersect` is a function of two args, rather than a method owned by one arg,
;   we now have an associative operation that we can apply over a list of args.
; for some operation *: ((x * y) * z) == (x * (y * z))
; we will do this reduction two ways.
; 1. across a pair of compartments.
;    This one is a bit trivial; 
;      each pair is len-2, so (reduce op pair) = (op pair[0] pair[1]),
;      but lets us keep consistent implementation across parts 1 and 2.
; 2. across "groups" of 3 rucksacks at a time.
;    This is where the reduction really helps.
;    Your alternative is a for loop or checking each case by hand.
;    NO NEED.
; when fns are owned by class instances you would never get to do this, cough cough.


; score fn, return the "priority" of a letter.
; this is a dict lookup.
; creating the dict interally is inefficient for multiple calls.
; nor do I want a global variable holding the dictionary.
; so I write a function from () to the dict and memoize it, so I create the dict only once.
(defn get_priority [letter] 
  (.get (cached_priority_map) letter))

; function from () to dict
; (keeps these variable bindings local)
(defn create_priority_map [] 
  (setv letters (list (map chr (range (ord "a") (+ (ord "z") 1)))))
  (setv LETTERS (list (map (fn [s] (.upper s)) letters)))
  (setv all_letters (+ letters LETTERS))
  (setv prio (range 1 (+ (len all_letters) 1)))
  (dict (zip all_letters prio)))

; cache to save repeated computation
(setv cached_priority_map (cache create_priority_map))



;;;;;;;;;;;;;;;;
;;    main    ;;
;;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))             ; create a "parser" instance
(.add_argument ps "-f" "--file")       ; expect a --file arg
(setv file (. (.parse_args ps) file))  ; unpack provided file to string

; each element of `data` is a rucksack.
; we divide it in two parts, get the intersecting set (should be len 1)
; and score the single char's priority
(defn part1 [rucksacks]
  (defn inner [sack]
      (setv inters (reduce intersect (partition_into 2 sack)))
      (assert (is_singleton inters))
      (get_priority (first inters)))
  ; sum prio for all sacks
  (sum (map inner rucksacks)))

; groups of 3, in a list
; reduce intersect over each triple, get prio of common element
(defn part2 [rucksacks]
  (setv groups (list (partition 3 rucksacks)))
  (defn inner [grp]
    (setv inters (reduce intersect grp))
    (assert (is_singleton inters))
    (get_priority (first inters)))
  (sum (map inner groups)))

(setv answers ((juxt part1 part2) (readlines file)))
(print answers)
