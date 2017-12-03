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
    its.samples.feat(bands = c("evi", "ndvi"))

# estimate accuracy
cerr_f.tb %>%
    its.ml.cross_validation(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 10),
                            cross = 5, cores = 4)
# Confusion Matrix and Statistics
#
# Reference
# Prediction         Cerrado.1 Cerrado.2 Cerrado_Campo Cerrado_Rupestre Corn_Cotton Fallow_Cotton.1 Fallow_Cotton.2 Forest Millet_Cotton Pasture Soy_Corn.1 Soy_Corn.2 Soy_Cotton.1
# Cerrado.1              291        29            14                6           0               0               0      0             0      17          0          0            0
# Cerrado.2               40       220             4                7           0               0               0      0             0      16          1          0            0
# Cerrado_Campo           27         7           419                9           0               0               0      0             0       8          0          0            0
# Cerrado_Rupestre        14        12             4              574           0               0               0      0             0       1          0          0            0
# Corn_Cotton              0         0             0                0          36               0               0      0             0       0          0          0            0
# Fallow_Cotton.1          0         0             0                0           0              24               0      0             0       0          1          0            3
# Fallow_Cotton.2          0         0             0                0           0               1             428      0             4       1          1          1           14
# Forest                   1         0             0                0           0               0               0    136             0       2          0          0            0
# Millet_Cotton            0         0             0                0           0               1               3      0           235       0          0          0            3
# Pasture                 27         9             8                1           0               3               1      2             0    1795         37          4            5
# Soy_Corn.1               0         0             0                0           0               0               1      0             0      36       1791         69           50
# Soy_Corn.2               0         0             0                0           0               0               0      0             0       5        239       2250           16
# Soy_Cotton.1             0         0             0                0           0               5               2      0             1       8         35          0          692
# Soy_Cotton.2             0         0             0                0           0               0               6      0             2       0         22         23          127
# Soy_Fallow               0         0             0                0           0               0               0      0             0       1          1          0            0
# Soy_Millet               0         0             0                0           0               0               0      0             0      11         31          3            1
# Soy_Sunflower            0         0             0                0           0               0               0      0             0       0          6          0            0
# Reference
# Prediction         Soy_Cotton.2 Soy_Fallow Soy_Millet Soy_Sunflower
# Cerrado.1                   0          0          1             0
# Cerrado.2                   0          0          0             0
# Cerrado_Campo               0          0          0             0
# Cerrado_Rupestre            0          0          0             0
# Corn_Cotton                 0          0          0             0
# Fallow_Cotton.1             0          0          0             0
# Fallow_Cotton.2             3          0          0             0
# Forest                      0          0          1             0
# Millet_Cotton               0          0          0             0
# Pasture                     3          3         24             0
# Soy_Corn.1                  7          2         63            13
# Soy_Corn.2                 21          0          2             1
# Soy_Cotton.1                9          0          4             0
# Soy_Cotton.2             2347          0          1             0
# Soy_Fallow                  0        133          3             0
# Soy_Millet                  1          1        146             2
# Soy_Sunflower               0          0          1            37
#
# Overall Statistics
#
# Accuracy : 0.9048
# 95% CI : (0.8996, 0.9098)
# No Information Rate : 0.1872
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.89
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
# Class: Cerrado.1 Class: Cerrado.2 Class: Cerrado_Campo Class: Cerrado_Rupestre Class: Corn_Cotton Class: Fallow_Cotton.1 Class: Fallow_Cotton.2 Class: Forest
# Sensitivity                   0.72750          0.79422              0.93318                 0.96147           1.000000               0.705882                0.97052       0.98551
# Specificity                   0.99458          0.99456              0.99586                 0.99745           1.000000               0.999686                0.99797       0.99968
# Pos Pred Value                0.81285          0.76389              0.89149                 0.94876           1.000000               0.857143                0.94481       0.97143
# Neg Pred Value                0.99122          0.99543              0.99756                 0.99811           1.000000               0.999215                0.99894       0.99984
# Prevalence                    0.03132          0.02169              0.03516                 0.04675           0.002819               0.002662                0.03453       0.01081
# Detection Rate                0.02279          0.01723              0.03281                 0.04495           0.002819               0.001879                0.03352       0.01065
# Detection Prevalence          0.02803          0.02255              0.03681                 0.04738           0.002819               0.002193                0.03547       0.01096
# Balanced Accuracy             0.86104          0.89439              0.96452                 0.97946           1.000000               0.852784                0.98425       0.99260
# Class: Millet_Cotton Class: Pasture Class: Soy_Corn.1 Class: Soy_Corn.2 Class: Soy_Cotton.1 Class: Soy_Cotton.2 Class: Soy_Fallow Class: Soy_Millet
# Sensitivity                       0.97107         0.9442            0.8273            0.9574             0.75960              0.9816           0.95683           0.59350
# Specificity                       0.99944         0.9883            0.9773            0.9727             0.99460              0.9826           0.99960           0.99601
# Pos Pred Value                    0.97107         0.9339            0.8814            0.8879             0.91534              0.9284           0.96377           0.74490
# Neg Pred Value                    0.99944         0.9902            0.9652            0.9902             0.98177              0.9957           0.99953           0.99205
# Prevalence                        0.01895         0.1489            0.1695            0.1840             0.07134              0.1872           0.01088           0.01926
# Detection Rate                    0.01840         0.1406            0.1403            0.1762             0.05419              0.1838           0.01042           0.01143
# Detection Prevalence              0.01895         0.1505            0.1591            0.1984             0.05920              0.1980           0.01081           0.01535
# Balanced Accuracy                 0.98526         0.9663            0.9023            0.9651             0.87710              0.9821           0.97822           0.79475
# Class: Soy_Sunflower
# Sensitivity                      0.698113
# Specificity                      0.999450
# Pos Pred Value                   0.840909
# Neg Pred Value                   0.998743
# Prevalence                       0.004150
# Detection Rate                   0.002897
# Detection Prevalence             0.003446
# Balanced Accuracy                0.848781

# train SVM model
its.predict <-
    cerr_f.tb %>%
    its.ml.create_predict(ml_model = its.ml.model.svm_radial(formula = its.formula.log(), cost = 10),
                          summation = c("rentropy"))

# save model to a RDS file
saveRDS(its.predict, file = "~/its.predict.rds")

# futher model applications:
# > its.predict(data_to_be_classified)
# where `data_to_be_classified` is massits feature tibble
