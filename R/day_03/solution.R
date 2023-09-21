# # imports
# box::use(purrr[compose, partial, map_int, map_chr, map])
library('purrr')
library('stringr')
# box::use(stringr[str_c, str_sub])

# some basic data
possible_letters = c(letters, toupper(letters))
priority = seq_along(possible_letters) |> setNames(possible_letters)
# priority as a function
get_priority = function(x) priority[x]

# split a string in half into list of vectors
split_sack = function(sack) {
    half_length = \(x) as.integer(nchar(x) / 2)
    compartments = c(str_sub(sack, start = 1L, end = half_length(sack)),
                     str_sub(sack, start = half_length(sack) + 1, end = -1L))
    return(compartments)
}

# smh ok, let's make this like a monoid (ish)
# for objects x and y, binary operation f(x,y) -> z is all elements in x also in y
# not exactly a monoid because it isn't important for us to preserve identity right now.
common_item = function(x, y) {
    splitx = strsplit(x, "")[[1]]
    matches = splitx %in% strsplit(y, '')[[1]] 
    splitx[matches] |> unique() |> str_c(collapse='') # collapsing ensures (m, m) -> m
}
# reduction on array gives us associativity, so this will work on > 2 inputs (part 2)
common_among = partial(Reduce, f = common_item)

# ----- part 1 ----------

# main
sacks = readLines(here::here("data", "03.txt"))
a1 = sacks |> map(split_sack) |> map_chr(common_among) |> map_int(get_priority) |> sum()

sacks |> map(split_sack) |> map(as.list) |> map(lift(common_item)) |> map(lift(get_priority)) |> lift(sum)()

# ----- part 2 ----------

# take a vector of length N and divide into N / n groups
groups_of = function(v_str, n) split(v_str, ceiling(seq_along(v_str) / n))

# do
groups_of(sacks, 3) |>
    map_chr(common_among) |>
    map_int(get_priority) |>
    sum() |>
    print()

if (FALSE) {
# test on vector of strings
    example = c("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw")
    test_answer = map(example, split_sack) |>
        map_chr(common_among) |>
        map_int(get_priority) |>
        sum()
    stopifnot(test_answer ==  157)

# test
    groups_of(example, 3) |>
        map_chr(common_among) |>
        map_int(get_priority) |>
        sum() |>
        print()
}



