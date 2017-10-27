
#' @title massits interpolation functions
#' @name its.interp.na
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Applies a function to some massits tibble's columns.
#' @param m             A valid massits tibble
#' @param interp        A string indicating what interpolation to use.
#'                      Can assume one of the following values:
#'                      \code{"last"}, \code{"linear"}, and \code{"spline"}
#'                      (Default \code{"spline"}).
#' @param bands         A string vector indicating those fields (bands)
#'                      to be interpolated. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @return Massits table
#' @export
its.interp.na <- function(m, interp = c("spline", "last", "linear"),
                          bands = its.bands()){
    fun <-
        if (interp[[1]] == "last"){
            function (x) return(zoo::na.locf(x, na.rm = FALSE))
        } else if (interp[[1]] == "linear"){
            function (x) return(zoo::na.approx(x, na.rm = FALSE))
        } else if (interp[[1]] == "spline"){
            function (x) return(zoo::na.spline(x, na.rm = FALSE))
        } else
            stop("its.interp.na - invalid interp parameter.")

    result <-
        dplyr::group_by(m, sample_id) %>%
        dplyr::do(its.apply(.its.stamp(.data), fun = fun, bands = bands)) %>%
        dplyr::ungroup()

    result <-
        result %>%
        .its.stamp()
    return(result)
}

#' @title massits interpolation functions
#' @name its.interp.k
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Applies a function to some massits tibble's columns.
#' @param m             A valid massits tibble
#' @param interp        A string indicating what interpolation to use.
#'                      Can assume one of the following values:
#'                      \code{"last"}, \code{"linear"}, and \code{"spline"}
#'                      (Default \code{"spline"}).
#' @param bands         A string vector indicating those fields (bands)
#'                      to be interpolated. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @param k             How many points to resample each time series.
#' @return Massits table
#' @export
its.interp.k <- function(m, interp = c("spline", "last", "linear"),
                         bands = its.bands(), k = 23){
    fun <-
        if (interp[[1]] == "last"){
            function (x) return(zoo::na.locf(x, na.rm = FALSE))
        } else if (interp[[1]] == "linear"){
            function (x) return(zoo::na.approx(x, na.rm = FALSE))
        } else if (interp[[1]] == "spline"){
            function (x) return(zoo::na.spline(x, na.rm = FALSE))
        } else
            stop("its.interp.na - invalid interp parameter.")

    result <- its.apply(m, fun = fun, bands = bands)

    return(result)
}
