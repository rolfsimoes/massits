#!/usr/bin/env Rscript

unserialize2 <- function(con){
    tryCatch({
        result <- unserialize(con)
    }, error = function(e) result <<- c())
    return(result)
}

to_tibble <- function(chunk.bin){
    print(tibble::as_tibble(chunk.bin))
}

map <- function(fun){
    cin <- file("stdin", "rb")
    while(TRUE){
        input <- unserialize2(cin)
        if (length(input) == 0)
            break
        fun(input)
    }
    close(cin)
}

map(to_tibble)
