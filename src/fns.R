
# from https://github.com/dgrtwo/adventdrob
# we don't use the pkg but we do do the session cookie trick

# You'll then have to set ADVENT_SESSION in your .Renviron to your Advent of Code cookie. Example of how to get that in Chrome:
#
# Visit adventofcode.com, and log in
# Right click + Inspect to view Developer Tools
# Select the Network tab
# Refresh the page, and select the "adventofcode.com" request
# Under Request Headers, there should be a cookie including session=<cookie here>. Copy that without the session=.

#' @export 
read_session <- function() {
    session = Sys.getenv('ADVENT_SESSION')
    if (session == '') {
        stop('session is missing from .Renviron')
    }
    return(session)
}

#' @export
advent_data <- function(year, day) {
    url = paste0('https://adventofcode.com/', year, '/day/', day, '/input')
    config_cookie = httr::set_cookies(session = read_session())
    req = httr::GET(url, config = config_cookie)
    content = httr::content(req, encoding = 'UTF-8')
    lines = stringr::str_split(content, '\n')[[1]]
    return(lines)
}


#' @export
matrix_of_chars <- function(x) {
    x |>
    strsplit('') |>
    unlist() |>
    matrix(byrow=TRUE, nrow=nchar(x)) |>
    t()
}

