
#' @title massits interpolation functions
#' @name its.interp.na
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Performs an interpolation on those measurements with NA value in
#'               some massits tibble's columns.
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
    its.valid(m, err_desc = "its.interp.na - invalid data input.")

    attrs <- attributes(m)[its.attrs]

    bands <- .its.produce(bands, m)

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
        .its.stamp() %>%
        its.apply(fun = fun, bands = bands) %>%
        dplyr::ungroup()

    result <-
        result %>%
        .its.stamp(attrs)
    return(result)
}
