box::use(fns = src/fns)

# ----- data ----------

d <- fns$advent_data(year=2022, day=1)
d <- as.numeric(d)

# for now we don't have to care about the NAs
ld <- d |>
    split(cumsum(is.na(d))) |>
    lapply(\(x) x[-1]) 


# ----- pt 1 ----------

cals <- lapply(ld, sum) |> as.numeric()
(a1 <- max(cals))


# ----- pt 2 ----------

# lol why don't I know what function does this in R
index <- function(x, q) {
    x[q]
}

tops <- cals |>
    sort(decreasing = TRUE) |>
    index(1:3)

(a2 <- sum(tops))
