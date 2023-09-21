box::use(fns = src/fns)
box::use(purrr[compose, partial, map_dbl])


# ----- data ----------

# head(-1) drops the newline at end-of-file (should be a rule maybe, or a fn)
(d <-
    fns$advent_data(year = 2022, day = 2) |>
    head(-1)
)

# ----- functions and mappers ----------

# this is useful metadata that we will recycle in many places
moves <- c("rock", "paper", "scissors")

# key-value pairs of code: shape
# it isn't strictly necessary to make these codes intelligible,
# but for human interface it is nice.
opp_map <- moves |> setNames(c("A", "B", "C"))
self_map <- moves |> setNames(c("X", "Y", "Z"))

# scores
shape_score <- c(1, 2, 3) |> setNames(moves)
outcome_score <- c("win" = 6, "draw" = 3, "lose" = 0)

# matrix of [opp, self] payoffs, with dimensions matching the order of {moves}
outcomes <- matrix(
    data <- c("draw", "win", "lose",
             "lose", "draw", "win",
             "win", "lose", "draw"),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(opp_map, self_map)
)


# parse a string of "A B" into c("A", "B")
parse_to_vector <- compose(unlist, partial(strsplit, split = " "))

# hash map lookups, I don't love the clunkiness of this
# this is why R needs tuples and unpacking
map_moves <- function(s) {
    c(opp_map[s[1]], self_map[s[2]])
}

play <- function(input) {
    codes <- parse_to_vector(input)            # to 2-vector of move codes
    ms <- map_moves(codes)                     # convert move codes to shapes
    result <- outcomes[ms[1], ms[2]]           # play RPS with shapes
   shape_score[ms[2]] + outcome_score[result]  # get round score
}


# ----- part 1 ----------

# test
ex_data <- c("A Y", "B X", "C Z")
map_dbl(ex_data, play) |> sum()

# final
map_dbl(d, play) |> sum()


# ----- part 2 ----------

outcome_map <- c("lose", "draw", "win") |> setNames(names(self_map))

pick_move_from_outcome <- function(input) {
    codes <- parse_to_vector(input)
    opp_move <- opp_map[codes[1]]
    result <- outcome_map[codes[2]]
    # vector of action: result given opp's move
    self_actions <- outcomes[opp_move, ]
    # get our chosen shape and outcome (named singleton vector)
    shape_outcome <- self_actions[which(self_actions == result)]
    # calculate score with name and value
    shape_score[names(shape_outcome)] + outcome_score[shape_outcome]
}

# test
map_dbl(ex_data, pick_move_from_outcome) |> sum()

# do
map_dbl(d, pick_move_from_outcome) |> sum()

