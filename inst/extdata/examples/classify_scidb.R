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

cores <- params[["cores"]]
if (is.null(cores))
    cores <- 1
cores <- as.integer(cores)

# read the predict model (SVM kernel radial, formula log)
its.predict <- readRDS("/net/esensing-001/disks/d9/scidb15_12/scripts/cerrado/model_classification_svm.rds")

# process
classify <- function(chunk.bin){
    result <- chunk.bin %>%
        tibble::as.tibble() %>%
        its(col_names = c("x", "y", "t")) %>%
        its.select(evi, ndvi, nir, mir) %>%
        its.apply_na() %>%
        its.interp.na() %>%
        its.scale() %>%
        its.translate() %>%
        its.feat(time_break = its.t_break(16, 23), cores = cores) %>%
        its.predict() %>%
        dplyr::select(-sample_id, -reference, -from, -to)
    return(result)
}

# unserialize
unserialize2 <- function(con){
    tryCatch({
        result <- unserialize(con)
    }, error = function(e) result <<- c())
    return(result)
}

# map_stream
map_stream <- function(fun){
    # open pipes
    cin <- file("stdin", "rb")
    cout <- pipe("cat", "wb")

    tryCatch({
        while(TRUE) {
            output <- list()
            input_data <- unserialize2(cin)

            if(length(input_data) == 0) {
                # write pipes
                writeBin(serialize(c(output), NULL, xdr=FALSE), cout)
                flush(cout)

                # close pipes
                close(cin)
                close(cout)
                break
            }

            output <- fun(input_data)

            # write pipes
            writeBin(serialize(c(output), NULL, xdr=FALSE), cout)
            flush(cout)
        }
    }, error = function(err){
        message(paste(date(), err$message, sep = ": "))
    }, finally = {
        tryCatch({
            # close pipes
            close(cin)
            close(cout)
        }, error = function(err){})
    })
}

# map each incoming chunk to classify function
map_stream(classify)
