# here's the idea:

# some "real work function" f: some atomic data -> value
# some "prettifier" g: value from f -> pretty value

# now apply this functorially:
# raw data -> iterable set of atomics -> iterable set of (g . f)(v) -> answer

library('purrr')

# lift is deprecated so I'll just recreate one myself :)
lift = \(f) partial(do.call, what = f)


# ----- g ----------

get_priority = function(x) {
    possible_letters = c(letters, toupper(letters))
    priority = seq_along(possible_letters) |> setNames(possible_letters)
    priority[x] |> setNames(NULL)
}

# ----- f ingredients ----------

# chop a string into a vector of chars
str_to_chars = function(x) strsplit(x, "")[[1]]

# a one-parameter version of str_sub
chop_string = function(x, at) {
    int_at = as.integer(at)
    fst = stringr::str_sub(x, 1L, int_at)
    snd = stringr::str_sub(x, int_at + 1, -1L)
    c(fst, snd)
}

# partial chop_string at n / 2
halve_string = function(x) {
    n = nchar(x)
    stopifnot(n %% 2 == 0)
    chop_string(x, n / 2)
}

common_among = function(a, b) a[a %in% b] |> unique()


# ----- do part 1 ----------

sacks = readLines(here::here("data", "03.txt"))
input_1 = sacks |> map(halve_string)

# for 1 element
sacks[1] |> halve_string() |> map(str_to_chars) |> lift(common_among)() |> get_priority()

# functorial application on list of elements
common_chars = compose(lift(common_among), \(x) map(x, .f = str_to_chars))
input_1 |> map(common_chars) |> map(get_priority) |> lift(sum)()

input_t |> map(compose(get_priority, common_chars)) |> lift(sum)()




sacks[1] |> halve_string() |> map(str_to_chars) |> lift(common_among)()

sacks[1] |> halve_string() |> common_chars()


sacks[1]
sacks[1] |> halve_string()



# let's not do this, let's encapsulate it instead
sacks |> map(halve_string) |> map(~ .x map(.x, .f = str_to_chars))


# string -> <char> -> list(<char>)
# divide a vector into a pair of half-length vectors
divide = function(x) {
    n = length(x)
    half_n = n / 2
    stopifnot(n %% 2 == 0)
    list(x[1:half_n], x[(half_n + 1):n])
}

index <- function(x, i) x[i]
compose <- purrr::compose
partial <- purrr::partial
map <- purrr::map

common_items = function(x, y) {
    compose(unique, partial(index, i = x %in% y))(x)
}

lift = function(f) partial(do.call, what = f)

common_among = partial(Reduce, f=common_items)

# ----- part 1 ----------

sacks |> map(str_to_chars) |> map(divide) |> purrr::flatten() |> common_among()

sacks |> map(str_to_chars) |> map(divide) |> map(lift(common_items)) |> map(get_priority) |> as.vector()

lift(common_items)


