box::use(./src/fns[advent_data, matrix_of_chars])
box::use(purrr[compose, partial])
box::use(dplyr[lag])

# data I/O nonsense
# matrix of chars => matrix of ints
fmap_int = partial(apply, MARGIN = 1, FUN = as.integer)
# file => lines => matrix of chars => matrix of ints
read_to_ints = compose(fmap_int, matrix_of_chars, readLines)

tst = c("30373", "25512", "65332", "33549", "35390") |> matrix_of_chars() |> fmap_int()
d = read_to_ints('data/08.txt')


# ----- pt 1: n. visible from each direction ----------

# this is the same as the "collecting water" l33tcode nonsense. 
# diff from cummax from each direction toward array maximum.

# array[numeric] => array[bool]; true if a[i] > a[i-1], a[i] = true
prior_max = compose(cummax, partial(lag, default = -1))
increases = function(x) (x - prior_max(x)) > 0
# the clever thing is to look from each direction, so we reverse before+after
right_increases = compose(rev, increases, rev)

# solve this by index-assigning bools into a zero matrix
n_visible = function(x) {
    val = 0 * x
    for (i in seq_len(nrow(x))) {
        tmp = x[i, ]
        val[i, ] = (val[i, ] | increases(tmp))
        val[i, ] = (val[i, ] | right_increases(tmp))
    }
    for (j in seq_len(ncol(x))) {
        tmp = x[, j]
        val[, j] = (val[, j] | increases(tmp))
        val[, j] = (val[, j] | right_increases(tmp))
    }
    return(val)
}

sum(n_visible(tst))
sum(n_visible(d))


# ----- pt 2 ----------

# for a slice of arr (including "itself")
# partial score for one "direction" from reference point
dir_score = function(arr) {
    n = head(arr, 1)
    not_itself = tail(arr, -1)
    too_tall = which(not_itself >= n)
    if (length(too_tall) == 0) {
        return(length(not_itself))
    } else {
        return(length(not_itself[1:min(too_tall)]))
    }
}

scenic_scores = function(x) {
    scores = 0 * x
    for (i in seq_len(nrow(scores))) {
        for (j in seq_len(ncol(scores))) {
            row = x[i, ]
            col = x[, j]
            left = dir_score(row[j:1])
            right = dir_score(row[j:length(row)])
            up = dir_score(col[i:1])
            down = dir_score(col[i:length(col)])
            scores[i, j] = left * right * up * down
        }
    }
    return(scores)
}

max(scenic_scores(tst))
max(scenic_scores(d))

