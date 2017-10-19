#' @title massits features functions
#' @name its.feat
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Transforms a massits tibble as an massits feature tibble
#' @param m             A valid massits tibble
#' @param bands         A valid string vector of band names
#' @param time_break    A numeric vector indicating the segments of time series to be breaked.
#'                      This vector can be computed in \code{\link{its.t_break}} function
#'                      (Default \code{its.t_break("2000-09-01", "12 months")}).
#' @param measure_id    A numeric vector indicating the measure sequences to compose the feature tibble
#' @param drop_na       Logical indicating wether to drop rows with any \code{NA} measured values (Default \code{TRUE}).
#' @param cores         A \code{multidplyr} argument to enable multithread processing.
#' @return Massits feature tibble
#' @export
its.feat <- function(m, bands, time_break = its.t_break("2000-09-01", "12 months"),
                     measure_id = NULL, drop_na = TRUE, cores = 1){

    its.valid(m, "its.feat - invalid data input.")

    m$time_break <-
        if (class(time_break) == "function"){
            time_break(m)
        } else {
            time_break
        }

    result <-
        dplyr::group_by(m, sample_id, time_break) %>%
        dplyr::mutate(t = 1:n()) %>%
        dplyr::ungroup()

    if (!is.null(measure_id))
        result <- dplyr::filter(result, t %in% measure_id)

    result <-
        dplyr::select(result, its.feat.cols, bands, time_break) %>%
        tidyr::gather_(key_col = "key", value_col = "value", gather_cols = bands)

    result <-
        result %>%
        dplyr::mutate(key = paste(key, sprintf(paste0("%0", nchar(max(t)), "d"), t), sep="."))

    result <-
        result %>%
        dplyr::mutate(t = time_break) %>%
        dplyr::select(-time_break)

    result <-
        if (cores > 1){
            cluster <-
                multidplyr::create_cluster(cores = cores) %>%
                multidplyr::cluster_library("tidyr")

            result %>%
                multidplyr::partition(sample_id, cluster = cluster) %>%
                dplyr::do(.data %>% (function(data.tb){
                    data.tb %>%
                        tidyr::spread(key, value)
                })) %>%
                dplyr::collect()
        } else {
            result %>%
                tidyr::spread(key, value)
        }

    if (drop_na)
        result <- tidyr::drop_na(result, -which(names(result) %in% its.feat.cols))

    result <-
        result %>%
        .its.feat.stamp()

    return(result)
}

#' @title massits features functions
#' @name its.feat.apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Applies a function to all feature tibble's attributes columns.
#' @param f             A valid massits measures tibble
#' @param fun           A valid function with one argument to be applied on each measure attribute.
#' @return Massits feature table
#' @export
its.feat.apply <- function(f, fun){
    its.feat.valid(f, "its.feat.apply - invalid data input.")
    result <- dplyr::mutate_at(f, -which(names(f) %in% its.feat.cols), fun)

    result <-
        result %>%
        .its.feat.stamp()
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
    result <-
        tidyr::drop_na(f, -which(names(f) %in% its.feat.cols)) %>%
        .its.feat.stamp()
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

    partitions <- sample(rep(seq_len(cross), each = ceiling(NROW(f) / cross))[1:NROW(f)])

    its.feat.fold <-
        function(i){
            result <-
                if (i >= 0){
                    dplyr::filter(f, partitions == i) %>%
                        .its.feat.stamp()
                } else {
                    dplyr::filter(f, partitions != -i) %>%
                        .its.feat.stamp()
                }
            return(result)
        }

    return(its.feat.fold)
}
