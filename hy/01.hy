; usage: 
; $ hy 01.hy -f path/to/file.txt

(import argparse [ArgumentParser])
(setv ps (ArgumentParser))
; take a file arg
(. ps (add_argument "-f" "--file"))
(setv args (. ps (parse_args)))

;;;;;;;;;;;;;;;;;;;;;
;;    functions    ;;
;;;;;;;;;;;;;;;;;;;;;

(import more_itertools [split_at])
(import operator [eq])
(import toolz [juxt flip pipe curry take compose compose_left])
(require hyrule [->])

; it's insane that this is what python makes you do btw
(defn readlines [f] 
  (with [o (open f "r")]
        (.readlines o)))

; divide a list at some delimiter value
(defn split_at_value [collection v]
  (split_at collection (curry eq v)))

; flip the signature of map: (map f d) -> (apply d f)
(setv apply (flip map))

; cast a list to list of ints
(defn list_to_ints [collection] 
  (list (map int collection)))

; descending sort
(setv sort_decreasing (compose reversed sorted))

;;;;;;;;;;;;;;;;
;;    main    ;;
;;;;;;;;;;;;;;;;

; -> is threading macro.
; effectively a pipe operator: (-> data f1 f2 f3) == (f3 (f2 (f1 data)))
(defn read_elf_data [filepath]
    (-> filepath (readlines)
       (split_at_value "\n")
       (apply list_to_ints)
       (list)))

; pre-compute the data but this isn't necessary, could have composed this step
(setv elves (read_elf_data (. args file)))

; part 1: given a list of lists of ints, calculate sum per list and return the largest sum
; this is a _function_
(setv part1 (compose max (curry map sum)))

; part 2: given a list of ints, calculate sum of the top 3 largest sums
; this is a _function_
(setv part2 
  (compose_left (curry map sum)
                sort_decreasing
                (curry take 3)
                sum))

; [fns] -> [values]
(setv solve_together (juxt part1 part2))

; get your answers
(print (solve_together elves))

