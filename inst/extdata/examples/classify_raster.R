#!/usr/bin/env Rscript

library(massits)

# train a predict model (SVM kernel radial, formula logarithm)
its.predict <-
    readRDS(system.file("extdata/data/mt.rds", package = "massits")) %>%
    its.select(evi, ndvi) %>%
    its.scale(10000) %>%
    its.apply_na() %>%
    its.interp.na() %>%
    its.translate(30000) %>%
    its.feat() %>%
    its.ml.create_predict(ml_model = its.ml.model.svm_radial(formula = its.formula.log()),
                          summation = "rentropy")

# open raster bricks with time series
chunk.tb <- its.raster(bands = list(evi = "~/Downloads/Sinop_evi.tif",
                                    ndvi = "~/Downloads/Sinop_ndvi.tif"),
                       chunk_size = 40 * 40)

# process
while(TRUE){
    chunk.tb <- chunk.tb %>%
        its.select(evi, ndvi) %>%
        its.apply_na() %>%
        its.interp.na() %>%
        its.translate(30000) %>%
        its.feat(time_break = its.t_break(16, 23)) %>%
        its.predict()

    chunk.tb %>%
        its.raster.save_chunk("~/Downloads/Sinop.tif",
                              overwrite = TRUE, save_bylayer = FALSE)

    if (its.raster.end(chunk.tb)) break
    chunk.tb <- its.raster.next_chunk(chunk.tb)
}
