; usage: 
; $ hy 01.hy -f path/to/file.txt

; this is done with deranged functional style

;;;;;;;;;;;;;;;;;;;;;
;;    functions    ;;
;;;;;;;;;;;;;;;;;;;;;

; ----- abstract things ----------

(import more_itertools [split_at])
(import operator [eq])
(import toolz [juxt flip curry take compose compose_left])
(require hyrule [->])

; it's insane that this is what python makes you do btw
(defn readlines [f] 
  (with [o (open f "r")]
        (.readlines o)))

; flip the signature of map: (map f d) -> (apply d f)
(setv apply (flip map))

; cast a list to list of ints
(defn list_to_ints [collection] 
  (list (map int collection)))

; descending sort
(setv sort_decreasing (compose reversed sorted))


; ----- problem-focused compositions ----------

; read lines into a nested list of ints.
; list of "elves", each elf is a list of ints (one int per "meal" ugh)
; I refuse to use classes but that would probably help me as pseudo-types.
;; ; -> is threading macro.
;; ; effectively a pipe operator: (-> data f1 f2 f3) == (f3 (f2 (f1 data)))
(defn read_elf_data [filepath]
  (-> filepath (readlines)
               (apply (fn [s] (.replace s "\n" "")))
               (split_at (curry eq ""))
               (apply (compose list (curry map int))) ; lmao
               (list)))

; part 1: [[int]] -> int
; nested list of ints -> sum per list -> largest sum
; this is a _function_
(setv part1 (compose max (curry map sum)))

; part 2: given a list of ints, calculate sum of the top 3 largest sums
; this is a _function_
(setv part2 
  (compose_left (curry map sum)
                sort_decreasing
                (curry take 3)
                sum))

;;;;;;;;;;;;;;;;
;;    main    ;;
;;;;;;;;;;;;;;;;

; parse a file from cmd line args
(import argparse [ArgumentParser])
(setv ps (ArgumentParser))          ; create a "parser" instance
(. ps (add_argument "-f" "--file")) ; expect a --file arg
(setv args (. ps (parse_args)))     ; arg values in a namespace
(setv file (. args file))           ; unpack provided file to string

; I am being obnoxious on purpose.
; juxt: (f1 f2) -> (x -> (val1, val2))
(setv compute_answers (juxt part1 part2))

(setv answers (compute_answers (read_elf_data file)))
(print answers)

