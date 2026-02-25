#' Financial year start date
#'
#' @param d a date, or vector of dates.
#'
#' @returns a date, or vector of dates for the 1st April in the relevant
#' financial year
#' @export
#'
#' @examples
#' f_year_start(Sys.Date())
f_year_start <- function(d) {
    d <- as.Date(d)
    y <- lubridate::year(d)
    if (lubridate::month(d) >= 4) {
        as.Date(sprintf("%d-04-01", y))
    } else {
        as.Date(sprintf("%d-04-01", y - 1))
    }
}



#' Financial year end date
#'
#' @param d a date, or vector of dates.
#'
#' @returns a date, or vector of dates for the 31st March in the relevant
#' financial year
#' @export
#'
#' @examples
#' f_year_end(Sys.Date())
f_year_end <- function(d) {
    d <- as.Date(d)
    y <- lubridate::year(d)
    if (lubridate::month(d) >= 4) {
        as.Date(sprintf("%d-03-31", y + 1))
    } else {
        as.Date(sprintf("%d-03-31", y))
    }
}



#' Financial year from date
#'
#' @param d a date, or vector of dates.
#' @param short TRUE/FALSE value, where TRUE returns 25/26 format, and FALSE
#' returns 2025/26. Default is FALSE.
#'
#' @returns a character value, or vector, of the form  24/25 for fiscal
#' year 2024/25
#' @export
#'
#' @examples
#' f_year(Sys.Date())
#' f_year(Sys.Date(), short = TRUE)
f_year <- function(d, short = FALSE) {
    d <- as.Date(d)
    y <- lubridate::year(d)

    if (short == TRUE) {
        modulo <- 100
    } else {
        modulo <- 10000
    }

    if (lubridate::month(d) >= 4) {
        as.character(paste0(sprintf("%02d", y %% modulo),"/", sprintf("%02d", (y + 1) %% 100)))
    } else {
        as.character(paste0(sprintf("%02d", (y - 1) %% modulo),"/", sprintf("%02d", y %% 100)))
    }
}
