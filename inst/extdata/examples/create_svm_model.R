#!/usr/bin/env Rscript

# This is an example of a R script that generates a SVM model for embrapa data
library(massits)

# open massits sample data
cerr.tb <-
    readRDS(system.file("extdata/data/data_dez2017.rds", package = "massits")) %>%
    dplyr::transmute(sample_id = 1:NROW(.), x = longitude, y = latitude, reference = label, ts = time_series) %>%
    tidyr::unnest() %>%
    dplyr::rename(t = Index) %>%
    its(); cerr.tb

# transforms it in massits features data
cerr.tb <-
    cerr.tb %>%
    its.scale(10000) %>%
    its.apply_na() %>%
    its.interp.na() %>%
    its.translate(30000) %>%
    its.apply(as.integer); cerr.tb

# create feture tibble
cerr_f.tb <-
    cerr.tb %>%
    its.samples.feat(bands = c("evi", "ndvi", "nir", "mir")); cerr_f.tb

# estimate accuracy
cerr_f.tb %>%
    its.ml.cross_validation(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 1000),
                            cross = 5, cores = 5)

# Confusion Matrix and Statistics
#
# Reference
# Prediction         Cerrado.1 Cerrado.2 Cerrado_Campo Cerrado_Rupestre Corn_Cotton Fallow_Cotton.1 Fallow_Cotton.2 Forest Millet_Cotton
# Cerrado.1              306        22            17                6           0               0               0      0             0
# Cerrado.2               41       235             3                5           0               0               0      0             0
# Cerrado_Campo           28         7           422                4           0               0               0      0             0
# Cerrado_Rupestre        15         3             2              582           0               0               0      0             0
# Corn_Cotton              0         0             0                0          36               0               0      0             0
# Fallow_Cotton.1          0         0             0                0           0              30               0      0             0
# Fallow_Cotton.2          0         0             0                0           0               0             424      0             5
# Forest                   1         0             0                0           0               0               0    135             0
# Millet_Cotton            0         0             0                0           0               0               2      0           233
# Pasture                  9        10             5                0           0               1               3      2             0
# Soy_Corn.1               0         0             0                0           0               2               0      0             0
# Soy_Corn.2               0         0             0                0           0               0               1      0             0
# Soy_Cotton.1             0         0             0                0           0               1               7      0             4
# Soy_Cotton.2             0         0             0                0           0               0               4      0             0
# Soy_Fallow               0         0             0                0           0               0               0      0             0
# Soy_Millet               0         0             0                0           0               0               0      1             0
# Soy_Sunflower            0         0             0                0           0               0               0      0             0
# Reference
# Prediction         Pasture Soy_Corn.1 Soy_Corn.2 Soy_Cotton.1 Soy_Cotton.2 Soy_Fallow Soy_Millet Soy_Sunflower
# Cerrado.1             12          0          0            0            0          0          2             0
# Cerrado.2              7          0          0            0            0          0          0             0
# Cerrado_Campo          5          0          0            0            0          0          0             0
# Cerrado_Rupestre       2          0          0            0            0          0          0             0
# Corn_Cotton            0          0          0            0            0          0          0             0
# Fallow_Cotton.1        0          2          0            0            0          0          0             0
# Fallow_Cotton.2        1          0          1           13            3          0          0             0
# Forest                 3          0          0            0            0          0          1             0
# Millet_Cotton          0          0          0            4            0          0          0             0
# Pasture             1815         42          4            6            2          1         18             0
# Soy_Corn.1            38       1865        119           39            8          1         64            12
# Soy_Corn.2             3        178       2198           12           20          0          3             0
# Soy_Cotton.1           5         25         10          728           69          0          3             0
# Soy_Cotton.2           0         13         16          109         2289          0          1             0
# Soy_Fallow             0          1          0            0            0        137          1             0
# Soy_Millet            10         35          2            0            0          0        153             1
# Soy_Sunflower          0          4          0            0            0          0          0            40
#
# Overall Statistics
#
# Accuracy : 0.9106
# 95% CI : (0.9055, 0.9155)
# No Information Rate : 0.1872
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.8968
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: Cerrado.1 Class: Cerrado.2 Class: Cerrado_Campo Class: Cerrado_Rupestre Class: Corn_Cotton
# Sensitivity                   0.76500          0.84838              0.93987                 0.97487           1.000000
# Specificity                   0.99523          0.99552              0.99643                 0.99819           1.000000
# Pos Pred Value                0.83836          0.80756              0.90558                 0.96358           1.000000
# Neg Pred Value                0.99242          0.99663              0.99781                 0.99877           1.000000
# Prevalence                    0.03132          0.02169              0.03516                 0.04675           0.002819
# Detection Rate                0.02396          0.01840              0.03305                 0.04558           0.002819
# Detection Prevalence          0.02858          0.02279              0.03649                 0.04730           0.002819
# Balanced Accuracy             0.88012          0.92195              0.96815                 0.98653           1.000000
#                      Class: Fallow_Cotton.1 Class: Fallow_Cotton.2 Class: Forest Class: Millet_Cotton Class: Pasture Class: Soy_Corn.1
# Sensitivity                        0.882353                0.96145       0.97826              0.96281         0.9548            0.8614
# Specificity                        0.999843                0.99813       0.99960              0.99952         0.9905            0.9733
# Pos Pred Value                     0.937500                0.94855       0.96429              0.97490         0.9463            0.8682
# Neg Pred Value                     0.999686                0.99862       0.99976              0.99928         0.9921            0.9718
# Prevalence                         0.002662                0.03453       0.01081              0.01895         0.1489            0.1695
# Detection Rate                     0.002349                0.03320       0.01057              0.01825         0.1421            0.1460
# Detection Prevalence               0.002506                0.03500       0.01096              0.01872         0.1502            0.1682
# Balanced Accuracy                  0.941098                0.97979       0.98893              0.98117         0.9726            0.9174
#                      Class: Soy_Corn.2 Class: Soy_Cotton.1 Class: Soy_Cotton.2 Class: Soy_Fallow Class: Soy_Millet Class: Soy_Sunflower
# Sensitivity                     0.9353             0.79912              0.9573           0.98561           0.62195             0.754717
# Specificity                     0.9792             0.98954              0.9862           0.99984           0.99609             0.999685
# Pos Pred Value                  0.9101             0.85446              0.9412           0.98561           0.75743             0.909091
# Neg Pred Value                  0.9853             0.98465              0.9901           0.99984           0.99260             0.998978
# Prevalence                      0.1840             0.07134              0.1872           0.01088           0.01926             0.004150
# Detection Rate                  0.1721             0.05701              0.1792           0.01073           0.01198             0.003132
# Detection Prevalence            0.1891             0.06672              0.1904           0.01088           0.01582             0.003446
# Balanced Accuracy               0.9572             0.89433              0.9718           0.99273           0.80902             0.877201

# train SVM model
its.predict <-
    cerr_f.tb %>%
    its.ml.create_predict(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 1000),
                          summation = c("none"))

# save model to a RDS file
saveRDS(its.predict, file = "~/its.predict.rds")

# futher model applications:
# > its.predict(data_to_be_classified)
# where `data_to_be_classified` is massits feature tibble
