#!/usr/bin/env Rscript

library(massits)

# read params
read_params <- function(){
    args <- commandArgs(TRUE)
    args_values <- lapply(args, function(arg){
        arg <- unlist(strsplit(arg, "="))
        arg <- unlist(strsplit(arg[2], ","))
        return(arg)
    })
    args_names <- sapply(args, function(arg){
        arg <- unlist(strsplit(arg, "="))
        return(arg[1])
    })
    names(args_values) <- args_names
    return(args_values)
}; params <- read_params()

arg_cost <- params[["cost"]]
arg_cross <- params[["cross"]]
if (is.null(arg_cost))
    cost <- 1
if (is.null(arg_cross))
    cross <- 5
arg_cost <- as.numeric(arg_cost)
arg_cross <- as.integer(arg_cross)

# train a predict model (SVM kernel radial, formula logarithm)
cross_validation <-
    readRDS("./data/pto_embrapa_rodrigo_michelle_damien.rds") %>%
    tidyr::unnest() %>%
    dplyr::select(-start_date, -end_date, -coverage) %>%
    its(col_names = c("x", "y", "reference", "t")) %>%
    its.select(evi, ndvi, nir, mir) %>%
    its.apply_na() %>%
    its.interp.na() %>%
    its.translate() %>%
    its.feat() %>%
    its.ml.cross_validation(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 10), cross = 5)

# print result
saveRDS(cross_validation, "./cross_validation.rds")
print(cross_validation)
