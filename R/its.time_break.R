
#' @title massits break functions
#' @name its.t_break
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Breaks the time series into segments regarding to \code{t_start} and
#'               \code{t_interval} parameters. A column \code{time_break} is
#'               created/updated with the identifier of corresponding time_series's segment.
#'               The massits tibble is then returned.
#' @param t_start       A valid time series index. Can be \code{numeric} or \code{lubridate::date}
#' @param t_interval    A valid interval to compose a theoretical time series breaking periods
#'                      Can be numeric if \code{from} is numeric or a \code{lubridate::period}
#'                      if \code{from} is \code{datetime}.
#' @param m             A valid massits tibble
#' @return Numeric vector
#' @export
its.t_break <- function(t_start, t_interval, m = NULL){
    result <-
        .its.factory(m, function(m){
            its.valid(m, "its.t_break - invalid data input.")

            sequence <-
                if (class(m$t) == "Date"){
                    seq(from = lubridate::as_date(t_start),
                        to   = max(m$t) + lubridate::period(t_interval),
                        by   = t_interval)
                } else {
                    seq(from = t_start,
                        to   = max(m$t) + t_interval,
                        by   = t_interval)
                }

            result <-
                cut(m$t,
                    sequence,
                    right = FALSE, labels = FALSE)

            return(result)
        })
    return(result)
}

#' @title massits break functions
#' @name its.t_break.count
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Counts the number of measures in each distinct pair of
#'               \code{sample_id} and \code{time_break}.
#' @param m             A valid massits tibble
#' @param time_break    A numeric vector indicating sequences of time series to be breaked.
#'                      This vector can be computed in \code{its.t_break()} function.
#' @return Table matrix
#' @export
its.t_break.count <- function(m, time_break){
    its.valid(m, "its.t_break.count - invalid data input.")

    m$time_break <- .its.produce(time_break, m)

    result <- table(m$sample_id, m$time_break)

    dim_names <- dimnames(result)
    names(dim_names) <- c("sample_id", "time_break")
    dimnames(result) <- dim_names

    return(result)
}

#' @title massits break functions
#' @name its.t_break.measures
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Counts the number of measures in each distinct measure sequence of each \code{time_break}.
#' @param m             A valid massits tibble
#' @param time_break    A numeric vector indicating sequences of time series to be breaked.
#'                      This vector can be computed in \code{its.t_break()} function.
#' @return Table matrix
#' @export
its.t_break.measures <- function(m, time_break){
    its.valid(m, "its.t_break.measures - invalid data input.")

    m$time_break <- .its.produce(time_break, m)

    m <-
        dplyr::group_by(m, sample_id, time_break) %>%
        dplyr::mutate(measure_id = 1:n())

    result <- table(m$measure_id)

    dim_names <- dimnames(result)
    names(dim_names) <- c("measure_id")
    dimnames(result) <- dim_names

    return(result)
}

#' @title massits break functions
#' @name its.t_break.dates
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Counts the number of measures in each distinct pair of
#'               \code{t} (time id) and the measure sequence of each \code{time_break}.
#' @param m             A valid massits tibble
#' @param time_break    A numeric vector indicating sequences of time series to be breaked.
#'                      This vector can be computed in \code{its.t_break()} function.
#' @return Table matrix
#' @export
its.t_break.dates <- function(m, time_break){
    its.valid(m, "its.t_break.dates - invalid data input.")

    m$time_break <- .its.produce(time_break, m)

    m <-
        dplyr::group_by(m, sample_id, time_break) %>%
        dplyr::mutate(measure_id = 1:n())

    result <-
        if (class(m$t) == "Date"){
            table(substr(m$t, 6, 11), m$measure_id)
        } else {
            table(m$t, m$measure_id)
        }

    dim_names <- dimnames(result)
    names(dim_names) <- c("t", "measure_id")
    dimnames(result) <- dim_names

    return(result)
}
