Hello World of Machine Learning
================
Lucas Argeles
7/14/2021

#### Install necessary packages

``` r
if (!require('caret')) 
{
  install.packages('caret');
  library(caret);
}
```

    ## Loading required package: caret

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
if (!require('ellipse')) 
{
  install.packages('ellipse');
}
```

    ## Loading required package: ellipse

    ## 
    ## Attaching package: 'ellipse'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     pairs

``` r
if (!require('e1071')) 
{
  install.packages('e1071');
}
```

    ## Loading required package: e1071

``` r
if (!require('kernlab')) 
{
  install.packages('kernlab');
}
```

    ## Loading required package: kernlab

    ## 
    ## Attaching package: 'kernlab'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     alpha

#### Load the data

``` r
data("iris")
dataset <- iris
```

#### Create validation dataset

What is a validation dataset?  
“Suppose that we would like to estimate the test error associated with
fitting a particular statistical learning method on a set of
observations. The validation set approach \[…\] is a very simple
strategy for this task. It involves randomly dividing the available set
of observations into two parts,a training set and a validation set or
hold-out set. The model is fit on the training set, and the fitted model
is used to predict the responses for the observations in the validation
set. The resulting validation set error rate— typically assessed using
MSE in the case of a quantitative response—provides an estimate of the
test error rate.”

— Gareth James, et al., Page 176, [An Introduction to Statistical
Learning: with Applications in R](https://amzn.to/2FoB4kf), 2013.

In essence, we have two datasets: one to use for prediction and one to
look at the prediction error rate to see how effecting our testing
methods were.

Here, we divide the datasets into two:

``` r
# create training index list of 80% of data to use later on
validation_index <- createDataPartition(dataset$Species, p=0.8, list = FALSE)
# create validation list of 20% of data
validation <- dataset[-validation_index,]
# use dataset for training
dataset <- dataset[validation_index,]
```

<br>

We will now look at the data six different ways:  
1. Dimensions of the dataset.  
2. Types of attributes.  
3. Peek at the data itself.  
4. Levels of the class attribute.  
5. Breakdown of the instances in each class.  
6. Statistical summary of all attributes.

##### Dimensions of the dataset

``` r
dim(dataset)
```

    ## [1] 120   5

Here, we see 120 instances (observations) and 5 attributes (variables).

##### Types of attributes

``` r
sapply(dataset, class)
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    ##    "numeric"    "numeric"    "numeric"    "numeric"     "factor"

Aside from the species attribute, we see that all other attributes are
numeric types.

##### First glance at the data

``` r
head(dataset)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

##### Levels of the class

``` r
levels(dataset$Species)
```

    ## [1] "setosa"     "versicolor" "virginica"

We observe three levels of species: setosa, versicolor, and virginica.  
Because there are three levels (more than two), this is classified as a
multinomial problem. If this dataset contained two levels, this would
have been referred to as a binary classification problem.

##### Class distribution

``` r
# Look a frequency of each specie as an absolute count
percentage <- prop.table(table(dataset$Species))*100
# Group species to see  frequency and percentages
cbind(freq = table(dataset$Species), percentage = percentage)
```

    ##            freq percentage
    ## setosa       40   33.33333
    ## versicolor   40   33.33333
    ## virginica    40   33.33333

Here, we see that the species are evenly distributed.

##### Statistical summary

``` r
summary(dataset)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.100   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.575   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.400   Median :1.300  
    ##  Mean   :5.832   Mean   :3.053   Mean   :3.745   Mean   :1.198  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.100   Max.   :6.700   Max.   :2.500  
    ##        Species  
    ##  setosa    :40  
    ##  versicolor:40  
    ##  virginica :40  
    ##                 
    ##                 
    ## 

#### Visualise dataset

Now we look at different visualizations that can give us a better idea
as to how the data is distributed.  
<br> We look at this via two different plots:  
1. Univariate plots to better understand each attribute  
2. Multivariate plots to understand the relationship among attributes

<br>

First we create the axes.

``` r
#create x and y of plots
x <- dataset[,1:4]
y <- dataset[,5]
```

##### Univariate plots

Then, we create box and whisker plot of each attribute. This is a visual
of the statistics we saw in the statistical summary above.

``` r
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We then look at frequency of species.

``` r
plot(y)
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

##### Multivarite plots

Now we look at how the attributes interact with each other.

We create a scatter plot which looks at trends in attribute pairs by
creating ellipses around each type of specie.

``` r
featurePlot(x=x, y=y, plot="ellipse")
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
<br> Here, we can observe clear relationships between attribute pairs
and class values.  
<br> We also observe box and whisker plots between each attribute.

``` r
featurePlot(x=x, y=y, plot = "box")
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
<br> Now it’s time to look at the distribution of each attribute by
class value. As such, we can observe bell curves of each attribute.

``` r
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density",
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            )
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

#### Evaluate data

Here, we test the data by creating models to estimate accuracy on unseen
data (data that has yet to be included).  
Here’s how we do this: 1. Set up test harness to use 10-fold cross
validation.  
2. Use 5 different models to predict species from flower measurements.  
3. Select the best model based on accuracy results.

##### Test Harness

``` r
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
```

##### Build models

We use 5 different algorithms:  
1. Linear Discriminant Analysis (LDA)  
2. Classification and Regression Trees (CART)  
3. k-Nearest Neighbors (kNN)  
4. Support Vector Machines (SVM) with a linear kernel  
5. Random Forest (RF)

``` r
# a) LINEAR algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) NONLINEAR algorithms
# Classification and Regression Trees
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# k-Nearest Neighbors
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) ADVANCED algorithms
# Support Vector Machines
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)
```

##### Select best model

We can report on the accuracy of each model by first creating a list of
the created models and using the summary function.

``` r
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: lda, cart, knn, svm, rf 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##           Min.   1st Qu.    Median      Mean 3rd Qu. Max. NA's
    ## lda  0.8333333 1.0000000 1.0000000 0.9833333       1    1    0
    ## cart 0.8333333 0.9166667 0.9583333 0.9416667       1    1    0
    ## knn  0.9166667 1.0000000 1.0000000 0.9833333       1    1    0
    ## svm  0.7500000 0.9166667 1.0000000 0.9500000       1    1    0
    ## rf   0.8333333 0.9375000 1.0000000 0.9666667       1    1    0
    ## 
    ## Kappa 
    ##       Min. 1st Qu. Median   Mean 3rd Qu. Max. NA's
    ## lda  0.750 1.00000 1.0000 0.9750       1    1    0
    ## cart 0.750 0.87500 0.9375 0.9125       1    1    0
    ## knn  0.875 1.00000 1.0000 0.9750       1    1    0
    ## svm  0.625 0.87500 1.0000 0.9250       1    1    0
    ## rf   0.750 0.90625 1.0000 0.9500       1    1    0

We then create a boxplot of the results for better visualization.

``` r
dotplot(results)
```

![](ML-Project--1---Iris-Flowers_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

We seem to get the highest accuracy results from the LDA model.  
<br> A summary of the model for the results are as followed:

``` r
print(fit.lda)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 120 samples
    ##   4 predictor
    ##   3 classes: 'setosa', 'versicolor', 'virginica' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 108, 108, 108, 108, 108, 108, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa
    ##   0.9833333  0.975

#### Make predictions

We then use the models to make predictions of the validation dataset we
created at the beginning.

``` r
#for LDA model
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
```

    ## Confusion Matrix and Statistics
    ## 
    ##             Reference
    ## Prediction   setosa versicolor virginica
    ##   setosa         10          0         0
    ##   versicolor      0          9         0
    ##   virginica       0          1        10
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9667          
    ##                  95% CI : (0.8278, 0.9992)
    ##     No Information Rate : 0.3333          
    ##     P-Value [Acc > NIR] : 2.963e-13       
    ##                                           
    ##                   Kappa : 0.95            
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: setosa Class: versicolor Class: virginica
    ## Sensitivity                 1.0000            0.9000           1.0000
    ## Specificity                 1.0000            1.0000           0.9500
    ## Pos Pred Value              1.0000            1.0000           0.9091
    ## Neg Pred Value              1.0000            0.9524           1.0000
    ## Prevalence                  0.3333            0.3333           0.3333
    ## Detection Rate              0.3333            0.3000           0.3333
    ## Detection Prevalence        0.3333            0.3000           0.3667
    ## Balanced Accuracy           1.0000            0.9500           0.9750
