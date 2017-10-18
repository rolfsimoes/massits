#!/usr/bin/env Rscript

# This is an example of a R script that generates a SVM model for embrapa data

# open massits sample data
mt.tb <- readRDS(system.file("extdata/data/mt.rds"))

# transforms it in massits features data
mt_f.tb <-
    mt.tb %>%
    its.feat(bands = c("ndvi", "evi", "nir", "mir"),
             time_break = its.t_break(mt.tb, "2000-09-01", "12 months"),
             drop_na = TRUE)

# estimate accuracy
mt_f.tb %>%
    its.feat.apply(function(x) x + 3) %>%
    its.ml.cross_validation(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 1000),
                            cross = 5)
#' Confusion Matrix and Statistics
#'
#'                Reference
#' Prediction      Cerrado Fallow_Cotton Forest Pasture Soy_Corn Soy_Cotton Soy_Fallow Soy_Millet Soy_Sunflower
#'   Cerrado           391             0      0       9        0          0          0          1             0
#'   Fallow_Cotton       0            33      0       0        1          2          0          0             0
#'   Forest              2             0    136       1        0          0          0          0             0
#'   Pasture             7             0      2     359        4          0          0          6             0
#'   Soy_Corn            0             1      0       1      349         17          0         27             4
#'   Soy_Cotton          0             0      0       0       14        379          0          3             0
#'   Soy_Fallow          0             0      0       0        0          0         88          0             0
#'   Soy_Millet          0             0      0       0       26          1          0        197             2
#'   Soy_Sunflower       0             0      0       0        4          0          0          1            47
#'
#' Overall Statistics
#'
#'                Accuracy : 0.9357
#'                  95% CI : (0.9244, 0.9458)
#'     No Information Rate : 0.1891
#'     P-Value [Acc > NIR] : < 2.2e-16
#'
#'                   Kappa : 0.9237
#'  Mcnemar's Test P-Value : NA
#'
#' Statistics by Class:
#'
#'                      Class: Cerrado Class: Fallow_Cotton Class: Forest Class: Pasture Class: Soy_Corn Class: Soy_Cotton
#' Sensitivity                  0.9775              0.97059       0.98551         0.9703          0.8769            0.9499
#' Specificity                  0.9942              0.99856       0.99848         0.9891          0.9709            0.9901
#' Pos Pred Value               0.9751              0.91667       0.97842         0.9497          0.8747            0.9571
#' Neg Pred Value               0.9947              0.99952       0.99899         0.9937          0.9714            0.9884
#' Prevalence                   0.1891              0.01608       0.06525         0.1749          0.1882            0.1887
#' Detection Rate               0.1849              0.01560       0.06430         0.1697          0.1650            0.1792
#' Detection Prevalence         0.1896              0.01702       0.06572         0.1787          0.1887            0.1872
#' Balanced Accuracy            0.9858              0.98457       0.99199         0.9797          0.9239            0.9700
#'                      Class: Soy_Fallow Class: Soy_Millet Class: Soy_Sunflower
#' Sensitivity                    1.00000           0.83830              0.88679
#' Specificity                    1.00000           0.98457              0.99758
#' Pos Pred Value                 1.00000           0.87168              0.90385
#' Neg Pred Value                 1.00000           0.97988              0.99709
#' Prevalence                     0.04161           0.11111              0.02506
#' Detection Rate                 0.04161           0.09314              0.02222
#' Detection Prevalence           0.04161           0.10686              0.02459
#' Balanced Accuracy              1.00000           0.91144              0.94218
#'

# train SVM model
its.predict <-
    mt_f.tb %>%
    its.feat.apply(function(x) x + 3) %>%
    its.ml.create_predict(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 1000),
                          summation = c("rentropy"))

# save model to a RDS file
saveRDS(its.predict, file = "~/its.predict.rds")

# futher model applications:
# > its.predict(data_to_be_classified)
# where `data_to_be_classified` is massits feature tibble
