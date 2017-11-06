its.raster.files.default <- function(base_path, file_regex, order_by_regex){
    files <-
        list.files(path = "~/MODIS_ARC/MODIS/MOD13Q1.006",
                   pattern = sprintf("MOD13Q1\\.A[0-9]{7}\\.h12v10\\..*\\.hdf"),
                   all.files = FALSE, full.names = TRUE, recursive = TRUE,
                   ignore.case = FALSE, include.dirs = TRUE, no.. = TRUE)
    files
}

its.raster.files.MODIS <- function(base_path, product, collection, tileH, tileV, start_date, end_date){}
