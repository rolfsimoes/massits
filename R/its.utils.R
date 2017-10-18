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
    if (is.null(m) | !("massits" %in% class(m)) | (NROW(m) < 1))
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
#' @return Massits tibble
.its.stamp <- function(m){
    inherits_class <- class(m)
    class(m) <- c("massits", inherits_class[(inherits_class != "massits")])
    return(m)
}

#' @title massits utils functions
#' @name .its.feat.stamp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Stamps a "features" class in the input tibble.
#' @param f            A valid massits features tibble
#' @return Massits tibble
.its.feat.stamp <- function(f){
    inherits_class <- class(f)
    class(f) <- c("features", inherits_class[(inherits_class != "features")])
    return(f)
}

#' @title massits utils functions
#' @name .its.predicted.stamp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Stamps a "features" class in the input tibble.
#' @param p            A valid massits predicted tibble
#' @return Massits tibble
.its.predicted.stamp <- function(p){
    inherits_class <- class(p)
    class(p) <- c("predicted", inherits_class[(inherits_class != "features") & (inherits_class != "predicted")])
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
    if (is.null(f) | !("features" %in% class(f)) | (NROW(f) < 1))
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
#' @name its.summary
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
