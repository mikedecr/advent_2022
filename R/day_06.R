# this would be way more interesting if R (and I) had more experience w/ data streams.

box::use(purrr[compose, partial])

chars = function(x) strsplit(x, '')[[1]]
n_unique = compose(length, unique)

is_unique_sequence = function(x, len) {
    stopifnot(length(x) == len)
    n_unique(x) == len
}

is_packet_start = partial(is_unique_sequence, len=4)
is_message_start = partial(is_unique_sequence, len=14)

find_first_sequence = function(x, len) {
    result = FALSE
    init = len - 1
    i = init
    while (result == FALSE) {
        i = i + 1
        consider = x[(i-init):i]
        result = is_unique_sequence(consider, len)
    }
    return(i)
}

# --------------------------------------------------
#  do it all
# --------------------------------------------------

d = readLines('data/06.txt')

# pt 1
find_first_sequence(chars(d), 4)
# pt 2
find_first_sequence(chars(d), 14)

