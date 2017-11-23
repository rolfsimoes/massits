#!/usr/bin/env Rscript

library(massits)

# train a predict model (SVM kernel radial, formula logarithm)
its.predict <-
    readRDS("./data/pto_embrapa_rodrigo_michelle_damien.rds") %>%
    tidyr::unnest() %>%
    dplyr::select(-start_date, -end_date, -coverage) %>%
    its(col_names = c("x", "y", "reference", "t")) %>%
    its.select(evi, ndvi, nir, mir) %>%
    its.apply_na() %>%
    its.interp.na() %>%
    its.translate() %>%
    its.feat() %>%
    its.ml.create_predict(ml_model = its.ml.model.svm_radial(formula = its.formula.log()),
                          summation = "rentropy")

# save to a file
saveRDS(its.predict, "./model_classification_svm.rds")
