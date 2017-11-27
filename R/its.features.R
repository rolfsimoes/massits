#' @title massits features functions
#' @name its.feat
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Transforms a massits tibble as an massits feature tibble
#' @param m             A valid massits tibble
#' @param bands         A valid string vector of band names. These names must coincide with
#'                      \code{m} columns (Default \code{its.bands()}).
#' @param time_break    A numeric vector indicating the segments of time series to be breaked.
#'                      This vector can be computed in \code{\link{its.t_break}} function
#'                      (Default \code{its.t_break("2000-09-01", "12 months")}).
#' @param measure_id    A numeric vector indicating the measure sequences to compose the feature tibble
#' @param drop_na       Logical indicating wether to drop rows with any \code{NA} measured values (Default \code{TRUE}).
#' @param cores         A \code{multidplyr} argument to enable multithread processing.
#' @return Massits feature tibble
#' @export
its.feat <- function(m, bands = its.bands(),
                     time_break = its.t_break("2000-09-01", "12 months"),
                     measure_id = NULL, drop_na = TRUE, cores = 1){

    its.valid(m, "its.feat - invalid data input.")

    attrs <- attributes(m)[its.attrs]

    bands <- .its.produce(bands, m)

    time_break <- .its.produce(time_break, m$t)
    t_length <- max(rle(time_break)$lengths)

    result <-
        purrr::map2(m[its.cols], its.cols, function(b, b_name){
            result <-
                b[!is.na(time_break)] %>%
                matrix(nrow = t_length) %>%
                t() %>% .[,1]
            result <- result %>%
                tibble::as_tibble()
            names(result) <- b_name
            return(result)
        }) %>%
        dplyr::bind_cols()

    time_range <-
        purrr::map2(m["t"], "t", function(b, b_name){
            result <-
                b[!is.na(time_break)] %>%
                matrix(nrow = t_length) %>%
                t() %>% .[, c(1, t_length)]
            result <- result %>%
                tibble::as_tibble()
            names(result) <- c("from", "to")
            return(result)
        }) %>%
        dplyr::bind_cols()

    features <-
        purrr::map2(m[bands], bands, function(b, b_name){
            result <-
                b[!is.na(time_break)] %>%
                matrix(nrow = t_length) %>%
                t()
            result <- result %>%
                tibble::as_tibble()
            names(result) <- paste(b_name, 1:NCOL(result), sep = ".")
            return(result)
        }) %>%
        dplyr::bind_cols()

    result <-
        list(result, time_range, features) %>%
        dplyr::bind_cols() %>%
        dplyr::select(its.feat.cols, dplyr::everything())

    if (drop_na)
        result <- tidyr::drop_na(result, -which(names(result) %in% its.feat.cols))

    result <-
        result %>%
        .its.feat.stamp(attrs)

    return(result)
}

#' @title massits utils functions
#' @name its.feat.drop_na
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Drop all rows with NA measures in \code{f} data.
#' @param f             A valid massits features tibble
#' @return Massits features tibble
#' @export
its.feat.drop_na <- function(f){
    its.feat.valid(f, "its.feat.length - invalid data input")

    attrs <- attributes(f)[its.attrs]

    fields <- which(!(names(f) %in% its.feat.cols))
    result <-
        tidyr::drop_na(f, fields) %>%
        .its.feat.stamp(attrs)
    return(result)
}

#' @title massits features functions
#' @name its.feat.create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return a function to select a partition of a massits features tibble.
#' @param f           A valid massits features tibble
#' @param cross       Number of partitions to be used in cross validation
#' @return Function to select a given partition
#' @export
its.feat.create_folds <- function(f, cross){
    its.feat.valid(f, "its.feat.create_folds - invalid data input.")

    attrs <- attributes(f)[its.attrs]

    partitions <- sample(rep(seq_len(cross), each = ceiling(NROW(f) / cross))[1:NROW(f)])

    its.feat.fold <-
        function(i){
            result <-
                if (i >= 0){
                    dplyr::filter(f, partitions == i) %>%
                        .its.feat.stamp(attrs)
                } else {
                    dplyr::filter(f, partitions != -i) %>%
                        .its.feat.stamp(attrs)
                }
            return(result)
        }

    return(its.feat.fold)
}
