#' @title Magrittr Pipe
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @description Magrittr compound assignment pipe-operator.
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
NULL

#' @title massits utils globals
#' @name its.samples.cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits tibble column names
its.samples.cols <-
    c("sample_id",
      "x",
      "y",
      "t",
      "reference")

#' @title massits utils globals
#' @name its.feat.cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits features tibble column names
its.feat.cols <-
    c("x",
      "y",
      "t")

#' @title massits utils globals
#' @name its.pred.cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits features tibble column names
its.pred.cols <-
    c("sample_id",
      "from",
      "to",
      "reference",
      "predicted")

#' @title massits utils globals
#' @name its.attrs
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits features tibble column names
its.attrs <-
    c("its_raster",
      "levels")

# massits global variables
utils::globalVariables(c(its.samples.cols,
                         its.feat.cols,
                         its.pred.cols,
                         "key",
                         "value",
                         "n",
                         ".data",
                         "."))

#' @title massits time series functions
#' @name its
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Create a new massits tibble loaded from \code{d} input.
#' @param d             A data source to be converted to massits tibble
#' @param col_names     A string vector informing corresponding valid column names of the input
#'                      as expected in a massive data tibble (Default \code{NULL}).
#' @return Massits tibble
#' @export
its <- function(d, col_names = NULL){

    m <- tibble::as_tibble(d)

    if (!(is.null(col_names))){
        col_names[1:min(length(names(m)), length(col_names))][is.null(col_names)] <- names(m)[1:min(length(names(m)), length(col_names))]
        names(m) <- c(col_names, names(m)[-length(col_names):0])
    }

    if (!("x" %in% names(m)))
        m$x <- as.double(NA)

    if (!("y" %in% names(m)))
        m$y <- as.double(NA)

    if (!("t" %in% names(m))){
        m$t <- as.double(NA)
    }

    if (!("reference" %in% names(m)))
        m$reference <- as.character(NA)

    m <- m %>%
        dplyr::select(its.samples.cols, dplyr::everything()) %>%
        .its.stamp()

    return(m)
}

#' @title massits time series functions
#' @name its.apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Applies a function to some massits tibble's fields
#' @param m             A valid massits tibble
#' @param fun           A valid function with one argument to be applied on each attribute
#'                      (its first argument).
#' @param bands_params  An optional parameter to be passed as \code{fun} second argument.
#'                      If informed, it must have \code{1} or the same number of bands.
#'                      (Default \code{NULL})
#' @param bands         A string vector indicating those fields (bands) to which
#'                      function \code{fun} will be applied. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @return Massits table
#' @export
its.apply <- function(m, fun, bands_params = NULL, bands = its.bands()){
    its.valid(m, "its.apply - invalid data input.")

    attrs <- attributes(m)[its.attrs]

    bands <- .its.produce(bands, m)

    if (!is.null(bands_params) &&
        length(bands_params) != 1 &&
        length(bands) != length(bands_params))
        stop("its.apply - `bands_params` must have the same length of `bands`")

    result <- m
    if (!is.null(bands_params)){
        if (length(bands_params) == 1)
            bands_params <- rep(bands_params, length(bands))
        for(i in seq_along(bands)){
            result <-
                dplyr::mutate_at(result,
                                 bands[[i]],
                                 function(d) fun(d, bands_params[[i]]))
        }
    } else
        result <- dplyr::mutate_at(result, bands, fun)

    result <-
        result %>%
        .its.stamp(attrs)
    return(result)
}

#' @title massits time series functions
#' @name its.apply_na
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Substitute some missing values by \code{NA} in massits tibble's fields.
#' @param m             A valid massits tibble
#' @param na_values     The missing value to be substituted by \code{NA}
#'                      (Default \code{-3000})
#' @param bands         A string vector indicating those fields (bands)
#'                      to be interpolated. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @return Massits table
#' @export
its.apply_na <- function(m, na_values = -3000, bands = its.bands()){

    result <- its.apply(m,
                        function(d, p){
                            d[d==p] <- NA
                            return(d)
                        },
                        bands = bands, bands_params = na_values)

    return(result)
}

#' @title massits time series functions
#' @name its.scale
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Scaling factor to transform values of massits tibble's fields.
#' @param m             A valid massits tibble
#' @param factors       Scaling factor to multiply all selected field values
#'                      (Default \code{0.0001}).
#' @param bands         A string vector indicating those fields (bands)
#'                      to be re-scaled. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @return Massits table
#' @export
its.scale <- function(m, factors = c(0.0001), bands = its.bands()){

    result <- its.apply(m, fun = function(d, p) return(d * p),
                        bands = bands, bands_params = factors)

    return(result)
}

#' @title massits time series functions
#' @name its.translate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Translation to be added in values of massits tibble's fields.
#' @param m             A valid massits tibble
#' @param amounts       Amount value to be added in all selected field values.
#'                      (Default \code{3}).
#' @param bands         A string vector indicating those fields (bands)
#'                      to be displaced. All non selected fields
#'                      will remain unchanged (Default \code{its.bands()}).
#' @return Massits table
#' @export
its.translate <- function(m, amounts = c(3), bands = its.bands()){

    result <- its.apply(m, fun = function(d, p) return(d + p),
                        bands = bands, bands_params = amounts)

    return(result)
}
