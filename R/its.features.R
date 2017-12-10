#' @title massits features functions
#' @name its.samples.feat
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Transforms a massits tibble as an massits feature tibble
#' @param m             A valid massits tibble
#' @param bands         A valid string vector of band names. These names must coincide with
#'                      \code{m} columns (Default \code{its.bands()}).
#' @return Massits feature tibble
#' @export
its.samples.feat <- function(m, bands = its.bands()){

    its.valid(m, "its.samples.feat - invalid data input.")

    attrs <- attributes(m)[its.attrs]

    bands <- .its.produce(bands, m)

    t_length <- dplyr::count(m, sample_id)$n
    if (any(t_length != t_length[1]))
        stop("its.samples.feat - all time series must have the same length.")
    t_length <- t_length[1]

    result <-
        purrr::map2(m[its.samples.cols], its.samples.cols, function(b, b_name){
            result <-
                b %>%
                matrix(nrow = t_length) %>%
                t() %>% .[,1]
            result <- result %>%
                tibble::as_tibble()
            names(result) <- b_name
            return(result)
        }) %>%
        dplyr::bind_cols()

    features <-
        purrr::map2(m[bands], bands, function(b, b_name){
            result <-
                b %>%
                matrix(nrow = t_length) %>%
                t()
            result <- result %>%
                tibble::as_tibble()
            names(result) <- paste(b_name, 1:NCOL(result), sep = ".")
            return(result)
        }) %>%
        dplyr::bind_cols()

    result <-
        list(result, features) %>%
        dplyr::bind_cols() %>%
        dplyr::select(its.samples.cols, dplyr::everything())

    result <-
        result %>%
        .its.feat.stamp(attrs)

    return(result)
}

#' @title massits features functions
#' @name its.raw.feat
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Transforms a massits tibble as an massits feature tibble
#' @param d             Any valid set of bands (i.e. named list of vectors)
#' @param t_length      The length of each data interval to be classified. The length of \code{d} must be
#'                      multiple of \code{t_length} (Default \code{23}).
#' @return Massits feature tibble
#' @export
its.raw.feat <- function(d, t_length = 23){

    if (any(is.null(names(d))))
        stop("its.raw.feat - data input must be named.")

    if (NROW(d) %% t_length != 0)
        stop("its.raw.feat - data length must be multiple of t_length.")

    bands <- names(d)
    bands <- bands[!(bands %in% its.feat.cols)]
    d$t <- rep(1:t_length, NROW(d) / t_length)

    result <-
        purrr::map2(d[its.feat.cols], its.feat.cols, function(b, b_name){
            result <-
                matrix(b, nrow = t_length) %>%
                t() %>% .[,1] %>%
                tibble::as_tibble()
            names(result) <- paste(b_name)
            return(result)
        }) %>%
        dplyr::bind_cols()

    features <-
        purrr::map2(d[bands], bands, function(b, b_name){
            result <-
                matrix(b, nrow = t_length) %>%
                t() %>%
                tibble::as_tibble()
            names(result) <- paste(b_name, 1:NCOL(result), sep = ".")
            return(result)
        }) %>%
        dplyr::bind_cols()

    na_headers <- its.samples.cols[!(its.samples.cols %in% its.feat.cols)]
    na_values <- as.integer(rep(NA, length(na_headers)))
    names(na_values) <- na_headers

    result <-
        list(result, features) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_(.dots = na_values) %>%
        dplyr::select(its.samples.cols, dplyr::everything())

    result <-
        result %>%
        .its.feat.stamp()

    return(result)
}

#' @title massits features functions
#' @name its.feat.create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return a function to select a partition of a massits features tibble.
#' @param f           A massits features tibble to compose the formula expression.
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
