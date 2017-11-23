#!/usr/bin/env Rscript

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

types <- params[["types"]]
if (is.null(types))
    types <- 'str'
types <- unlist(strsplit(types, ","))

# predefined types
types_list <- list("str"=as.character, "int32"=as.integer, "double"=as.numeric)
if (!all(types %in% names(types_list)))
    stop("Error: types argument must be a list of 'str', 'int32', and 'double'.")

# open pipes
cin  <- file("stdin", "r")
cout <- pipe("cat", "wb")

iquery_to_tibble <- function(input){
    input <- gsub("(\\{(.*)\\} )(.*)", "\\3", input)
    input <- input[1:(length(input)-1)]
    lines_split <- strsplit(input, ",")
    header <- lines_split[[1]]
    data_mask <- lapply(header, function(x) header == x)
    names(data_mask) <- header
    input <- unlist(lines_split[2:length(lines_split)])
    input <- tibble::as_tibble(lapply(data_mask, function(x) input[x]))
    
    if (length(types) == 1)
        types <- rep(types, NCOL(input))

    if (length(types) != NCOL(input))
        stop("Error: types list must have same columns of input data.")

    input <- dplyr::bind_cols(purrr::map2(input, types, function(d, t){ return(types_list[[t]](d)) }))
    return(input)
}

tryCatch({
    while(TRUE) {
        output <- list()
        input_data <- readLines(cin)

        if(length(input_data) == 0) {
            # write pipes
            writeBin(serialize(c(output), NULL, xdr=FALSE), cout)
            flush(cout)
                
            # close pipes
            close(cin)
            close(cout)
            break
        }

        output <- iquery_to_tibble(input_data)

        # write pipes
        writeBin(serialize(c(output), NULL, xdr=FALSE), cout)
        flush(cout)
    }
}, error = function(err){
    message(paste(date(), err$message, sep = ": "))
}, finally = {
    # close pipes
    close(cin)
    close(cout)
})
