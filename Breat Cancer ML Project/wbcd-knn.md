Wisconsin Breat Cancer Data ML Project
================
Lucas Argeles
7/15/2021

Data is a collection of 469 patients that were found with breast cancer.
Among those, data has been provided as to whether the patient’s cancer
was benign or malignant. Below, we use the kNN function to see if this
model can accurately predict whether a patient’s cancer is benign or
malignant in the future.

## Use kNN method for machine learning

First we import the data.

``` r
library(readr)
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
# we see 569 examples and 32 features
```

We drop the id column.

``` r
wbcd <- wbcd[-1]
```

We then observe the malignant vs. benign examples.

``` r
table(wbcd$diagnosis)
```

    ## 
    ##   B   M 
    ## 357 212

We give more meaningful information to the table by adding labels to the
diagnosis column.

``` r
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
```

We look at distribution of B vs. M in percentages.

``` r
round(prop.table(table(wbcd$diagnosis)) *100, digits = 1)
```

    ## 
    ##    Benign Malignant 
    ##      62.7      37.3

We take a peak at the summary of three different features in the table.
This gives us a general idea of the data we are dealing with.

``` r
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
```

    ##   radius_mean       area_mean      smoothness_mean  
    ##  Min.   : 6.981   Min.   : 143.5   Min.   :0.05263  
    ##  1st Qu.:11.700   1st Qu.: 420.3   1st Qu.:0.08637  
    ##  Median :13.370   Median : 551.1   Median :0.09587  
    ##  Mean   :14.127   Mean   : 654.9   Mean   :0.09636  
    ##  3rd Qu.:15.780   3rd Qu.: 782.7   3rd Qu.:0.10530  
    ##  Max.   :28.110   Max.   :2501.0   Max.   :0.16340

Because the area mean ranges from 143.5 to 2501 and smoothness ranges
from .05 to .16, normalization needs to be applied.

``` r
# create normalize function
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x))) }
# apply normalize function to the wbcd data
wbcd_n <- as.data.frame( lapply(wbcd[2:31], normalize))
```

We then split the newly normalized set into two separate datasets: one
training set and one testing set. Because the data set is already
randomly organized, we can safely use the first 469 observations for
training. If this dataset were in chronological order, a random sampling
methods would be necessary.

``` r
# training set uses the first 469 observations
wbcd_train <- wbcd_n[1:469,]
# testing set uses the remaining observations
wbcd_test <- wbcd_n[470:569,]
```

Because the **diagnosis** column was omitted, we need to create a new
vector to be able to store these labels.

``` r
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
```

We then install the class package to use the kNN function.

``` r
if (!require('class')) 
{
  install.packages('class');
  library(class);
}
```

    ## Loading required package: class

``` r
if (!require('gmodels')) 
{
  install.packages('gmodels');
  library(gmodels);
}
```

    ## Loading required package: gmodels

We finally apply the kNN function to the test data.

``` r
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=21)
```

We then create a cross tabulation between two vectors to analyze the
agreement.

``` r
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  100 
    ## 
    ##  
    ##                  | wbcd_test_pred 
    ## wbcd_test_labels |    Benign | Malignant | Row Total | 
    ## -----------------|-----------|-----------|-----------|
    ##           Benign |        61 |         0 |        61 | 
    ##                  |     1.000 |     0.000 |     0.610 | 
    ##                  |     0.968 |     0.000 |           | 
    ##                  |     0.610 |     0.000 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##        Malignant |         2 |        37 |        39 | 
    ##                  |     0.051 |     0.949 |     0.390 | 
    ##                  |     0.032 |     1.000 |           | 
    ##                  |     0.020 |     0.370 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##     Column Total |        63 |        37 |       100 | 
    ##                  |     0.630 |     0.370 |           | 
    ## -----------------|-----------|-----------|-----------|
    ## 
    ## 

Here we can see that 61 cases were benign, 37 cases were malignant, and
2 cases that were predicted as malignant though were actually benign.  
<br> We now look at different ways to improve our previous classifier.
We will try a different method for rescaling our numeric features and
then try different values for k.  
We see if applying a z-score scaling could help us avoid false positives
or false negatives.

``` r
wbcd_z <- as.data.frame(scale(wbcd[-1])) # omit the diagnosis column
summary(wbcd_z)
```

    ##   radius_mean       texture_mean     perimeter_mean      area_mean      
    ##  Min.   :-2.0279   Min.   :-2.2273   Min.   :-1.9828   Min.   :-1.4532  
    ##  1st Qu.:-0.6888   1st Qu.:-0.7253   1st Qu.:-0.6913   1st Qu.:-0.6666  
    ##  Median :-0.2149   Median :-0.1045   Median :-0.2358   Median :-0.2949  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.4690   3rd Qu.: 0.5837   3rd Qu.: 0.4992   3rd Qu.: 0.3632  
    ##  Max.   : 3.9678   Max.   : 4.6478   Max.   : 3.9726   Max.   : 5.2459  
    ##  smoothness_mean    compactness_mean  concavity_mean     points_mean     
    ##  Min.   :-3.10935   Min.   :-1.6087   Min.   :-1.1139   Min.   :-1.2607  
    ##  1st Qu.:-0.71034   1st Qu.:-0.7464   1st Qu.:-0.7431   1st Qu.:-0.7373  
    ##  Median :-0.03486   Median :-0.2217   Median :-0.3419   Median :-0.3974  
    ##  Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.63564   3rd Qu.: 0.4934   3rd Qu.: 0.5256   3rd Qu.: 0.6464  
    ##  Max.   : 4.76672   Max.   : 4.5644   Max.   : 4.2399   Max.   : 3.9245  
    ##  symmetry_mean      dimension_mean      radius_se         texture_se     
    ##  Min.   :-2.74171   Min.   :-1.8183   Min.   :-1.0590   Min.   :-1.5529  
    ##  1st Qu.:-0.70262   1st Qu.:-0.7220   1st Qu.:-0.6230   1st Qu.:-0.6942  
    ##  Median :-0.07156   Median :-0.1781   Median :-0.2920   Median :-0.1973  
    ##  Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.53031   3rd Qu.: 0.4706   3rd Qu.: 0.2659   3rd Qu.: 0.4661  
    ##  Max.   : 4.48081   Max.   : 4.9066   Max.   : 8.8991   Max.   : 6.6494  
    ##   perimeter_se        area_se        smoothness_se     compactness_se   
    ##  Min.   :-1.0431   Min.   :-0.7372   Min.   :-1.7745   Min.   :-1.2970  
    ##  1st Qu.:-0.6232   1st Qu.:-0.4943   1st Qu.:-0.6235   1st Qu.:-0.6923  
    ##  Median :-0.2864   Median :-0.3475   Median :-0.2201   Median :-0.2808  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.2428   3rd Qu.: 0.1067   3rd Qu.: 0.3680   3rd Qu.: 0.3893  
    ##  Max.   : 9.4537   Max.   :11.0321   Max.   : 8.0229   Max.   : 6.1381  
    ##   concavity_se       points_se        symmetry_se       dimension_se    
    ##  Min.   :-1.0566   Min.   :-1.9118   Min.   :-1.5315   Min.   :-1.0960  
    ##  1st Qu.:-0.5567   1st Qu.:-0.6739   1st Qu.:-0.6511   1st Qu.:-0.5846  
    ##  Median :-0.1989   Median :-0.1404   Median :-0.2192   Median :-0.2297  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.3365   3rd Qu.: 0.4722   3rd Qu.: 0.3554   3rd Qu.: 0.2884  
    ##  Max.   :12.0621   Max.   : 6.6438   Max.   : 7.0657   Max.   : 9.8429  
    ##   radius_worst     texture_worst      perimeter_worst     area_worst     
    ##  Min.   :-1.7254   Min.   :-2.22204   Min.   :-1.6919   Min.   :-1.2213  
    ##  1st Qu.:-0.6743   1st Qu.:-0.74797   1st Qu.:-0.6890   1st Qu.:-0.6416  
    ##  Median :-0.2688   Median :-0.04348   Median :-0.2857   Median :-0.3409  
    ##  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.5216   3rd Qu.: 0.65776   3rd Qu.: 0.5398   3rd Qu.: 0.3573  
    ##  Max.   : 4.0906   Max.   : 3.88249   Max.   : 4.2836   Max.   : 5.9250  
    ##  smoothness_worst  compactness_worst concavity_worst    points_worst    
    ##  Min.   :-2.6803   Min.   :-1.4426   Min.   :-1.3047   Min.   :-1.7435  
    ##  1st Qu.:-0.6906   1st Qu.:-0.6805   1st Qu.:-0.7558   1st Qu.:-0.7557  
    ##  Median :-0.0468   Median :-0.2693   Median :-0.2180   Median :-0.2233  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.5970   3rd Qu.: 0.5392   3rd Qu.: 0.5307   3rd Qu.: 0.7119  
    ##  Max.   : 3.9519   Max.   : 5.1084   Max.   : 4.6965   Max.   : 2.6835  
    ##  symmetry_worst    dimension_worst  
    ##  Min.   :-2.1591   Min.   :-1.6004  
    ##  1st Qu.:-0.6413   1st Qu.:-0.6913  
    ##  Median :-0.1273   Median :-0.2163  
    ##  Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.4497   3rd Qu.: 0.4504  
    ##  Max.   : 6.0407   Max.   : 6.8408

We then repeat the aforementioned steps with the newly scaled data.

``` r
# training set uses the first 469 observations
wbcd_train <- wbcd_z[1:469,]
# testing set uses the remaining observations
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  100 
    ## 
    ##  
    ##                  | wbcd_test_pred 
    ## wbcd_test_labels |    Benign | Malignant | Row Total | 
    ## -----------------|-----------|-----------|-----------|
    ##           Benign |        61 |         0 |        61 | 
    ##                  |     1.000 |     0.000 |     0.610 | 
    ##                  |     0.924 |     0.000 |           | 
    ##                  |     0.610 |     0.000 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##        Malignant |         5 |        34 |        39 | 
    ##                  |     0.128 |     0.872 |     0.390 | 
    ##                  |     0.076 |     1.000 |           | 
    ##                  |     0.050 |     0.340 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##     Column Total |        66 |        34 |       100 | 
    ##                  |     0.660 |     0.340 |           | 
    ## -----------------|-----------|-----------|-----------|
    ## 
    ## 

This time around, we happened to get more false positives than before.
<br> We can conclude that the kNN method is useful in the general sense
but need to keep its nickname in mind as the **lazy predictor** and its
margin for error. This method of machine learning does not, in fact,
involve machine learning. It simply stores the training data verbatim.
