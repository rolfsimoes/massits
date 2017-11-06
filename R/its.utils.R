#' @title massits utils functions
#' @name its.valid
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Tests if \code{m} is a valid massits data.
#'               Returns \code{TRUE} if the input is valid. Else, returns \code{FALSE} or
#'               throws an exception if \code{err_desc} is informed.
#' @param m            A valid massits tibble
#' @param err_desc     An error description to be returned.
#' @return Logical
#' @export
its.valid <- function(m, err_desc = NULL){
    if (is.null(m) | !all(its.cols %in% names(m)) | (NROW(m) < 1))
        if (!is.null(err_desc)){
            stop(err_desc)
        } else
            return(FALSE)
    return(TRUE)
}

#' @title massits utils functions
#' @name .its.stamp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Stamps a "massits" class in the input tibble.
#' @param m            A valid massits tibble
#' @param attrs        Any additional attributes to include in the massits tibble.
#' @return Massits tibble
.its.stamp <- function(m, attrs = NULL){
    inherits_class <- class(m)
    class(m) <- c("massits", inherits_class[(inherits_class != "massits")])

    if (!is.null(attrs))
        attributes(m)[names(attrs)] <- attrs[names(attrs)]

    return(m)
}

#' @title massits utils functions
#' @name .its.feat.stamp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Stamps a "features" class in the input tibble.
#' @param f            A valid massits features tibble
#' @param attrs        Any additional attributes to include in the massits features tibble.
#' @return Massits tibble
.its.feat.stamp <- function(f, attrs = NULL){
    inherits_class <- class(f)
    class(f) <- c("features", inherits_class[(inherits_class != "features")])

    if (!is.null(attrs))
        attributes(f)[names(attrs)] <- attrs[names(attrs)]

    return(f)
}

#' @title massits utils functions
#' @name its.pred.valid
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Tests if \code{p} is a valid massits predicted data.
#'               Returns \code{TRUE} if the input is valid. Else, returns \code{FALSE} or
#'               throws an exception if \code{err_desc} is informed.
#' @param p            A valid massits tibble
#' @param err_desc     An error description to be returned.
#' @return Logical
#' @export
its.pred.valid <- function(p, err_desc = NULL){
    if (is.null(p) | !all(its.pred.cols %in% names(p)) | (NROW(p) < 1))
        if (!is.null(err_desc)){
            stop(err_desc)
        } else
            return(FALSE)
    return(TRUE)
}

#' @title massits utils functions
#' @name .its.pred.stamp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Stamps a "features" class in the input tibble.
#' @param p            A valid massits predicted tibble
#' @param attrs        Any additional attributes to include in the massits predicted tibble.
#' @return Massits tibble
.its.pred.stamp <- function(p, attrs = NULL){
    inherits_class <- class(p)
    class(p) <- c("predicted", inherits_class[(inherits_class != "features") & (inherits_class != "predicted")])

    if (!is.null(attrs))
        attributes(p)[names(attrs)] <- attrs[names(attrs)]

    return(p)
}

#' @title massits utils functions
#' @name its.feat.valid
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Tests if \code{f} is a valid massits feature data.
#'               Returns \code{TRUE} if the input is valid. Else, returns \code{FALSE} or
#'               throws an exception if \code{err_desc} is informed.
#' @param f             A valid massits features tibble
#' @param err_desc      An error description to be returned.
#' @return Logical
#' @export
its.feat.valid <- function(f, err_desc = NULL){
    if (is.null(f) | !all(its.feat.cols %in% names(f)) | (NROW(f) < 1))
        if (!is.null(err_desc)){
            stop(err_desc)
        } else
            return(FALSE)
    return(TRUE)
}

#' @title massits utils functions
#' @name its.raster.valid
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Tests if \code{r} is a valid massits raster data.
#'               Returns \code{TRUE} if the input is valid. Else, returns \code{FALSE} or
#'               throws an exception if \code{err_desc} is informed.
#' @param r             A valid massits features tibble
#' @param err_desc      An error description to be returned.
#' @return Logical
#' @export
its.raster.valid <- function(r, err_desc = NULL){
    if (is.null(r) | !("its_raster" %in% class(r)))
        if (!is.null(err_desc)){
            stop(err_desc)
        } else
            return(FALSE)
    return(TRUE)
}

#' @title massits utils functions
#' @name its.summary
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return a summary of massits data input.
#' @param m           A valid massits tibble
#' @return Summary tibble
#' @export
its.summary <- function(m){
    its.valid(m, "its.summary - invalid data input.")

    ref.tb <-
        m %>%
        dplyr::group_by(sample_id, reference) %>%
        dplyr::summarise()

    result.tb <-
        table(ref.tb$reference, useNA = "ifany") %>%
        tibble::as_tibble()

    names(result.tb) <- c("reference", "n")

    return(result.tb)
}

#' @title massits utils functions
#' @name its.feat.length
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return the length of measures in \code{f} data.
#' @param f             A valid massits features tibble
#' @return Numeric
#' @export
its.feat.length <- function(f){
    its.feat.valid(f, "its.feat.length - invalid data input")
    result <- NCOL(f) - length(its.feat.cols)
    return(result)
}

#' @title massits utils functions
#' @name its.bands
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return the bands of massits \code{m} data.
#' @param m           A valid massits tibble
#' @param but         A vector indicating bands names to not return in result
#' @return A string vector with all bands
#' @export
its.bands <- function(m = NULL, but = c("from", "to")){
    result <- .its.factory(m, function(m){
        its.valid(m, "its.bands - invalid data input")
        result <- colnames(m)
        result <- result[!(result %in% c(its.cols, but))]
        return(result)
    })
    return(result)
}

#' @title massits utils functions
#' @name .its.factory
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return an evaluation of a given function or an enclosure.
#' @param x           A valid massits or massits features tibble
#' @param its_fun     A function that receives as argument an massitis (features) tibble
#' @return Function evaluation or an enclosed function
.its.factory <- function(x = NULL, its_fun){
    if (is.null(x))
        return(its_fun)
    return(its_fun(x))
}

#' @title massits utils functions
#' @name .its.produce
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return an evaluation of a given function factory.
#' @param its_fac     A value or function that receives as argument an
#'                    massitis (features) tibble. This function generally is
#'                    generated by \code{.its.factory} function.
#' @param x           A valid massits or massits features tibble
#' @return Values returned by a function evaluation
.its.produce <- function(its_fac, x){
    if (class(its_fac) == "function")
        return(its_fac(x))
    return(its_fac)
}
#' @title massits utils functions
#' @name its.select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Select bands of a massits \code{m} data.
#' @param m           A valid massits tibble
#' @param ...         Bands names arguments to return.
#' @return A massits tibble corresponding with selected bands.
#' @export
its.select <- function(m = NULL, ...){

    dots <- substitute(list(...))
    dots_names <- (sapply(dots, class) == "name")
    bands <- sapply(dots, function(x){
        if (class(x) == "name")
            return(deparse(x))
        else if (class(x) == "call")
            tryCatch(return(eval(x)),
                     error = function(e)
                             stop("its.select - invalid bands expression."))
    })[-1:0] %>% unlist()

    result <- .its.factory(m, function(m){
        its.valid(m, "its.select - invalid data input.")

        if (!all(bands %in% its.bands(m)))
            stop("its.select - invalid bands.")

        result <- dplyr::select_(m, .dots = c(its.feat.cols[its.feat.cols %in% colnames(m)], bands))
        return(result)
    })
    return(result)
}
