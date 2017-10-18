#' @import magrittr
#'

#' @title massits utils globals
#' @name its.cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits tibble column names
its.cols <-
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
    c("sample_id",
      "x",
      "y",
      "t",
      # "from",
      # "to",
      "reference")

#' @title massits utils globals
#' @name its.class.cols
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Massits features tibble column names
its.pred.cols <-
    c("sample_id",
      "x",
      "y",
      "t",
      # "from",
      # "to",
      "reference",
      "predicted")

# massits global variables
utils::globalVariables(c(its.cols,
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
#' @description  Create a new massits tibble loaded from \code{x} input.
#' @param x             A data source to be converted to massits tibble
#' @param col_names     A string vector informing corresponding valid column names of the input
#'                      as expected in a massive data tibble
#' @return Massits tibble
#' @export
its <- function(x, col_names = NULL){

    m <- tibble::as_tibble(x)

    if (!(is.null(col_names)))
        names(m) <- c(col_names, names(m)[-length(col_names):0])

    if (!("sample_id" %in% names(m)))
        m <-
            m %>%
            dplyr::mutate(sample_id = dplyr::group_indices(m, x, y))

    if (!("reference" %in% names(m)))
        m$reference <- as.character(NA)

    m <-
        m %>%
        dplyr::select(its.cols, dplyr::everything()) %>%
        .its.stamp()

    return(m)
}
