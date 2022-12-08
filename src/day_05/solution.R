# --------------------------------------------------
#  utilities
# --------------------------------------------------

box::use(purrr[compose])
pprint = \(x) cat(x, sep = '\n')


# --------------------------------------------------
#  string shaping
# --------------------------------------------------

# ----- crates ----------

# gets the top chunk out of the input
isolate_crate_chunk = function(x) {
    empty = which(x == '')
    head(x, empty)
}

# we use this for the top section of crates, but it could be useful in other AoC tasks
# turn a set of equal-width strings into a matrix of characters
matrix_of_chars = function(x) {
    x |> strsplit('') |> unlist() |> matrix(nrow=unique(nchar(x))) |> t()
}

list_of_crates = function(x) {
    # the crate "index" was at the bottom of the grid, but we want it to be at the top
    x = x[nrow(x):1, ]
    # remove index row and any column that is blank
    x = x[-1, which(x[1, ] != ' ')] 
    # convert remaining columns into lists
    x = x |> as.data.frame() |> as.list()
    # and remote missing cells from each list
    x = x |> lapply(\(z) z[which(z != ' ')])
    return(x)
}

format_crates = compose(list_of_crates, matrix_of_chars, isolate_crate_chunk)


isolate_moves_chunk = function(x) {
    empty = which(x == '')
    tail(x, length(x) - empty)
}

remove = function(x, str) {
    lapply(x, function(x) stringr::str_replace_all(x, str, ''))
}

# list of 'move', 'from' and 'to'
format_moves = function(x) {
    x |>
    isolate_moves_chunk() |>
    remove('move ') |>
    remove('from ') |>
    remove('to ') |>
    (function(x) lapply(x, strsplit, ' '))() |>
    lapply(unlist) |>
    lapply(as.integer) |>
    lapply(setNames, nm = c('move', 'from', 'to'))
}

# --------------------------------------------------
#  algo to move crates
# --------------------------------------------------

# crate lifting order is dependent on the "crane version"
lift_crates = function(crates, n, from, crane=9000) {
    lifted = tail(crates[[from]], n)
    if (crane == 9000) lifted = rev(lifted)
    return(lifted)
}

# move crates from one stack to another
move_crates = function(crates, n, from, to, crane=9000) {
    moving = lift_crates(crates, n, from, crane)
    crates[[from]] = head(crates[[from]], -n)
    crates[[to]] = c(crates[[to]], moving)
    return(crates)
}

# move crates according to a list of instuctions
apply_moves = function(crates, moves, crane) {
    # escape hatch for recursive function
    if (length(moves) == 0) return(crates)
    # this simulates a "pop"
    p = head(moves, 1)[[1]]
    moves = tail(moves, -1)
    # execute move and call recursively
    crates = move_crates(crates, p['move'], p['from'], p['to'], crane)
    apply_moves(crates, moves, crane)
}

# additional data IO stuff
solution = function(d, crane) {
    final = apply_moves(format_crates(d), format_moves(d), crane=crane)
    final |>
        lapply(function(x) x[[length(x)]]) |>
        unlist() |>
        stringr::str_c(collapse='')
}

# --------------------------------------------------
#  pt 1
# --------------------------------------------------

test = readLines('data/05_test.txt')
d = readLines('data/05.txt')

solution(test, 9000)
solution(test, 9001)

solution(d, 9000)
solution(d, 9001)

