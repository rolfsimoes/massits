#' @title massits raster functions
#' @name its.raster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Creates a new massits raster data from a list of files.
#'               The object can be used in its.raster function family.
#' @param bands         A named list with all raster files, e.g. \code{band_name="file_path"}.
#' @param chunk_size    A raw number or a function informing the number of pixels to
#'                      be computed. The function receives as its argument the raster brick object.
#'                      This number is rounded to rows (Default \code{4900} \eqn{(70x70)}).
#' @return Massits tibble with an \code{its_raster} object in its attributes.
#' @export
its.raster <- function(bands = list(evi = "~/Downloads/sinop-crop-evi.tif",
                                    ndvi = "~/Downloads/sinop-crop-ndvi.tif"),
                       chunk_size = 4900){

    chunk_size <- .its.produce(chunk_size, template)

    bands <-
        bands %>%
        lapply(function(x){
            if (is.character(x))
                return(raster::brick(x))
            stop("its.raster - invalid bands")
        })

    r <- list()
    r$template <- bands[[1]]
    r$bands <- bands
    r$chunk_size <- raster::blockSize(r$template, chunk_size, minblocks = 1)
    r$next_chunk <- 1
    r$save_bands <- NULL
    class(r) <- c("its_raster")

    result <- .its.raster.next_chunk(r)

    return(result)
}

#' @title massits raster functions
#' @name .its.raster.chunk
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Loads a chunk of data from a valid massits raster. If there is no
#'               more chunk to load from rasters, throws an exception.
#'               Check if there are chunks to be read with \code{\link{its.raster.end}} function.
#' @param r              A valid \code{its_raster} object.
#' @return Massits tibble with all data of current chunk.
.its.raster.next_chunk <- function(r){

    its.raster.valid(r, err_desc = "its.raster.next_chunk - `chunk.tb` does not have raster object")

    if (r$next_chunk > r$chunk_size$n){
        r$next_chunk <- 1
        stop("its.raster.next_chunk - end of chunks")
    }

    i <- r$next_chunk

    result <-
        r$bands %>%
        purrr::map(function(x) {
            result <-
                raster::getValuesBlock(x,
                                       row = r$chunk_size$row[[i]],
                                       nrows = r$chunk_size$nrows[[i]]) %>% c()
            return(result)
        }) %>%
        tibble::as_tibble()

    result$sample_id <- rep(1:(r$chunk_size$nrows[[i]] * raster::ncol(r$template)) +
                                (r$chunk_size$row[[i]] - 1) * raster::ncol(r$template),
                            raster::nbands(r$template))
    result$x <- rep(rep(1:raster::ncol(r$template),
                        r$chunk_size$nrows[[i]]),
                    raster::nbands(r$template))
    result$y <- rep(rep(1:r$chunk_size$nrows[[i]] + (r$chunk_size$row[[i]] - 1),
                        each = raster::ncol(r$template)),
                    raster::nbands(r$template))
    result$t <- rep(1:raster::nbands(r$template),
                    each = r$chunk_size$nrows[[i]] * raster::ncol(r$template))
    result$reference <- as.character(NA)

    r$next_chunk <- r$next_chunk + 1

    attrs <- list("its_raster" = r)
    result <-
        result %>%
        dplyr::select(c(its.cols, names(r$bands))) %>%
        .its.stamp(attrs)

    return(result)
}

#' @title massits raster functions
#' @name its.raster.chunk
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Loads a chunk of data from a valid massits raster. If there is no
#'               more chunk to load from rasters, throws an exception.
#'               Check if there are chunks to be read with \code{\link{its.raster.end}} function.
#' @param chunk.tb       A valid massits tibble created by \code{\link{its.raster}} function.
#'                       This tibble must have an \code{its_raster} object in its attribute.
#' @return Massits tibble with all data of current chunk.
#' @export
its.raster.next_chunk <- function(chunk.tb){

    r <- attr(chunk.tb, "its_raster")

    result <- .its.raster.next_chunk(r)

    return(result)
}

#' @title massits raster functions
#' @name its.raster.end
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Check if there are chunks to be loaded in \code{r} massits raster.
#' @param chunk.tb       A valid massits tibble created by \code{\link{its.raster}} function.
#'                       This tibble must have an \code{its_raster} object in its attribute.
#' @return Logical
#' @export
its.raster.end <- function(chunk.tb){

    r <- attr(chunk.tb, "its_raster")
    its.raster.valid(r, err_desc = "its.raster.end - `chunk.tb` does not have raster object")

    result <- r$next_chunk > r$chunk_size$n
    return(result)
}

#' @title massits raster functions
#' @name its.raster.save_chunk
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Save chunks of data to files in GTiff format.
#'               The prefix of file names is the informed \code{file}.
#'               This function looks for \code{its_raster} attribute of
#'               \code{chunk.tb} input.
#' @param chunk.tb      A valid massits predicted tibble.
#' @param file          A file path to be used as base name of files to be saved.
#' @param overwrite     Logical indicating if file can be overitten (Default \code{FALSE}).
#' @param save_bylayer  Save each time period as a separated file (Default \code{TRUE}).
#' @param save_bands    Bands to be saved (Default \code{its.bands}).
#' @return The same input massits predicted tibble.
#' @export
its.raster.save_chunk <- function(chunk.tb, file = "~/Downloads/test.tif",
                                  overwrite = FALSE, save_bylayer = TRUE,
                                  save_bands = its.bands()){

    r <- attr(chunk.tb, "its_raster")

    its.raster.valid(r, "its.raster.save_chunk - no valid `its_raster` object informed.")

    save_bands <- .its.produce(save_bands, chunk.tb)
    if (!all(save_bands %in% its.bands(chunk.tb)))
        stop("its.raster.save_chunk - invalid bands.")

    nlayers <- max(chunk.tb$t)

    if (is.null(r$save_bands)){
        ext <- raster::extent(r$template)

        r$save_bands <-
            save_bands %>%
            purrr::map(function(x){
                raster::brick(nrows = nrow(r$template),
                              ncols = ncol(r$template),
                              xmn = ext@xmin,
                              xmx = ext@xmax,
                              ymn = ext@ymin,
                              ymx = ext@ymax,
                              nl = nlayers,
                              crs = raster::crs(r$template)) %>%
                    raster::writeStart(file, overwrite = overwrite)
            })
        names(r$save_bands) <- save_bands
    }

    bands_values <-
        chunk.tb %>%
        dplyr::arrange(t, y, x) %>%
        dplyr::select(save_bands) %>%
        lapply(function(x) matrix((if (is.factor(x)) as.integer(x) else x), ncol = nlayers))

    for (band in save_bands){
        r$save_bands[[band]] <-
            raster::writeValues(r$save_bands[[band]],
                                bands_values[[band]],
                                start = r$chunk_size$row[[r$next_chunk - 1]])
    }

    if (its.raster.end(chunk.tb)){
        message("its.raster.save_chunk - saving bands in separated files...")

        for (band in save_bands){
            r$save_bands[[band]] <- raster::writeStop(r$save_bands[[band]])

            r$save_bands[[band]] <-
                raster::writeRaster(r$save_bands[[band]],
                                    filename = gsub("(^.*)(\\..+)",
                                                    sprintf("\\1_%s\\2", band),
                                                    file),
                                    format = "GTiff",
                                    overwrite = overwrite,
                                    bylayer = save_bylayer,
                                    suffix = "numbers")
        }
        if (file.exists(file))
            file.remove(file)
    }

    return(chunk.tb)
}
