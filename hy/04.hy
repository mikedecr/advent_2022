(import toolz [last first
               curry juxt
               compose_left])
(import operator [contains eq])


; ----- clean each line from raw data ----------

; dumb that you have to do this
(defn rstrip [f] (.rstrip f))
(defn split_at [pattern string] (.split string pattern))

; convert "a-b" to (range a (+ b 1))
(defn parse_range [string]
  (setv ints (tuple (map int (.split string "-"))))
  (assert (eq (len ints) 2))
  (range (first ints) (+ (last ints) 1)))

; each line has a pair (tuple) of ranges
; read to a list of such lines
(defn readlines [filepath]
  (setv lines (with [f (open filepath "r")] (list (map rstrip f))))
  (setv clean (compose_left (curry split_at ",")
                            (curry map parse_range)
                            tuple))
  (list (map clean lines)))


; ----- problem library ----------

; true when (any/all) b elements are in a
; provide any/all as :how
(defn contains_how [a b how]
  (setv in_a (curry contains a))
  (how (map in_a b)))

; for a pair a b, check how_contains both directions
(defn either_contains_how [a b how]
  (or (contains_how a b how)
      (contains_how b a how)))

; unpack a pair to separate sets and evaluate inclusion
(defn pair_either_contains_how [pair how]
  (either_contains_how (unpack-iterable pair) how))


;;;;;;;;;;;;;;;
;;    main   ;;
;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))             ; create a "parser" instance
(.add_argument ps "-f" "--file")       ; expect a --file arg
(setv file (. (.parse_args ps) file))  ; unpack provided file to string

(defn part1 [section_assignments]
  (setv total_containments
        (map (curry pair_either_contains_how :how all) section_assignments))
  (sum total_containments))

(defn part2 [section_assignments]
  (setv partial_containments
        (map (curry pair_either_contains_how :how any) section_assignments))
  (sum partial_containments))

(setv answers ((juxt part1 part2) (readlines file)))
(print answers)

