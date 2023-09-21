
item = `[[`

parse_instruction = function(x) stringr::str_split(x, ' ')[[1]]

compute_signal <- function(X, queue, deferred = character(0)) {
    if (length(deferred) > 0) {
        X <- X + as.numeric(deferred[[1]])
    }
    to_defer <- tail(deferred, -1)
    if (length(queue) == 0) {
        return(X)
    }
    instruction <- strsplit(queue[[1]], " ")
    if (instruction[[1]][1] == 'addx') {
        to_defer[length(to_defer) + 1] = instruction[[1]][2]
    }
    compute_signal(X, tail(queue, -1), to_defer)
}



increment_signal = function(x, held, input) {
    if (length(held) != 0) {
        ret <- x + held[[1]]
    } else {
        ret <- x
    }

    instruction = strsplit(que[[1]], ' ')
    rest = tail(que, -1)
    if (instruction[1] == 'addx') {
        ldef = length(deferred)
        deferred[ldef] = as.numeric(instruction[2])
    }
}


# --------------------------------------------------
#  main
# --------------------------------------------------

d <- readLines(here::here("data", "10_test.txt"))

get_add_value = function(x) {
    stopifnot(x != 'addx 0')
    instruction = strsplit(x, " ")[[1]]
    if (instruction[1] == 'noop')
        return(0)
    return(as.numeric(instruction[2]))
}

pad_cycles = function(stream) {
    # determine needed length
    adds = sum(grepl('addx', stream))
    values = vector(mode = 'numeric', length = length(stream) + adds)
    logs = list()
    vv = 1
    for (ii in seq_along(stream)) {
        item = stream[ii]
        instruction = strsplit(item, " ")[[1]]
        values[vv] = 0
        if (instruction[1] == 'addx') {
            vv = vv + 1
            values[vv] = as.numeric(instruction[2])
        }
        if ((ii - 20) %% 40 == 0) {
            logs[length(logs) + 1] = vv * cumsum(values)[vv]
        }
        vv = vv + 1
    }
    return(list(values, logs))
}



cumsum(pad_cycles(d))[c(20, 60)]





head(as.list(d), 3 - 1)



x = 'noop'
x |> strsplit(" ") |> item(1) |> item(2) |> as.numeric()

strsplit(x " ")[[1]][2]

lapply(strsplit(d, " "), function(x) min(0 & x[2])

parse_number = function(x) {
    chars = strsplit(x, "")[[1]]
    nums = as.numeric(chars)
    nums[!is.na(nums)]
}

as.numeric(d)

logs = list()
x = 1
deferred = list()
for (ii in seq_along(d)) {
    if (length(deferred) > 0)
        x = x + deferred
    parsed = strsplit(d[ii], " ")
    x = x + deferred
    if (parsed[1] == 'addx') {
        next
    }

}

for (ii in seq_along(d)) {
    parsed = strsplit(d[ii], " ")[[1]]
    if (parsed[1] != 'noop') {
        deferred = as.numeric(parsed[2])
    }
    if (ii == 20 | ((ii - 20) %% 40 == 0)) {
        print(ii)
        logs[length(logs) + 1] = x * ii
    }
    if (length(deferred) > 0)
        x = x + deferred
}

