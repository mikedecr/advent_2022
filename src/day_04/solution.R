box::use(fns = ./src/fns)
box::use(purrr[map, partial, compose, map_int])

compose_right = partial(compose, .dir='forward')


# uglee data cleaning routine
chop = function(x, sep) strsplit(x, sep)[[1]]

sanitize_input = function(x) {
    chop(x, sep = ',') |>
    map(.f = chop, sep = '-') |>
    map(.f = as.integer)
}

# this comes in handy :)
# lift a function from args to list of args
lift = function(f) partial(do.call, what=f)


# ----- do part 1 ----------

any_contained = function(a, b) {
    # safety: check length of a and b
    stopifnot((length(a) == 2) & (length(b) == 2))
    # i hate this
    b_in_a = (a[1] <= b[1] & a[2] >= b[2])
    a_in_b = (a[1] >= b[1] & a[2] <= b[2])
    any(b_in_a, a_in_b)
}


zones = readLines('data/04.txt')

zones |>
    map(sanitize_input) |>
    map(.f = lift(any_contained)) |>
    lift(sum)()


# ----- do part 2 ----------

# c(a, b) -> a:b
range_of = function(a, b) a:b

any_overlap = function(a, b) {
    # safety: check length of a and b
    stopifnot((length(a) == 2) & (length(b) == 2))
    # elementwise TRUE if any elements are in common
    arange = lift(range_of)(as.list(a))
    brange = lift(range_of)(as.list(b))
    sum(c(arange %in% brange, brange %in% arange)) >= 1
}

zones |>
    map(sanitize_input) |>
    map(.f = lift(any_overlap)) |>
    lift(sum)()


