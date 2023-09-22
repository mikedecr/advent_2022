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

; for each b, assess its membership in a
; and accumulate with some `how` function.

; The joy here is a procedural abstraction.
; the function doesn't know what `how` is and doesn't have to.
; in practice, we will pass `:how any` or `:how all` to reduce the inclusions to a bool
; as we will see, that will be the only difference between parts 1 and 2
(defn contains_how [a b how]
  (setv in_a (curry contains a))
  (how (map in_a b)))

; for a pair a b, check how_contains in both directions
; (a in b) or (b in a)
(defn either_contains_how [a b how]
  (or (contains_how a b how)
      (contains_how b a how)))


;;;;;;;;;;;;;;;
;;    main   ;;
;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))             ; create a "parser" instance
(.add_argument ps "-f" "--file")       ; expect a --file arg
(setv file (. (.parse_args ps) file))  ; unpack provided file to string

; modifies signature of either_contains to take a pair; inject `how`, and sum
(defn count_containments [assignments how]
  (setv pair_contains
    (fn [pair] (either_contains_how (unpack-iterable pair) how)))
  (setv containments (map pair_contains assignments))
  (sum containments))

; feels good
(setv part1 (curry count_containments :how all))
(setv part2 (curry count_containments :how any))

(setv answers ((juxt part1 part2) (readlines file)))
(print answers)

