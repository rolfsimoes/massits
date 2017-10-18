#' @title massits formula functions
#' @name its.formula.linear
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Create a formula to be used in some Machine Learning models
#'               with predictors from massits features tibble \code{f} atributes.
#' @param f             A massits features tibble to compose the formula expression.
#' @return Formula
#' @export
its.formula.linear <- function(f = NULL){
    result <-
        .its.factory(f, function(f){
            its.feat.valid(f)
            predictors <- paste(names(f)[-length(its.feat.cols):0], collapse = " + ")
            result = stats::formula(paste("factor(reference) ~", predictors))
            return(result)
        })

    return(result)
}

#' @title massits formula functions
#' @name its.formula.log
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Create a formula to be used in some Machine Learning models
#'               with predictors from massits features tibble \code{f} atributes.
#' @param f             A massits features tibble to compose the formula expression.
#' @return Formula
#' @export
its.formula.log <- function(f = NULL){
    result <-
        .its.factory(f, function(f){
            predictors <- paste(paste0("log(", names(f)[-length(its.feat.cols):0], ")"), collapse = " + ")
            result = stats::formula(paste("factor(reference) ~", predictors))
            return(result)
        })

    return(result)
}
