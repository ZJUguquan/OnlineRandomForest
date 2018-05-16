Introduction
============

Online Random Forest(ORF) was first introduced by [Amir Saffari, etc](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.150.1671&rep=rep1&type=pdf). After that, [Arthur Lui](https://github.com/luiarthur/ORFpy) has implemented the algorithm using Python. Many thanks for their work. Following the paper's idea and Lui's implemention, I refactor the code via R and R6 package. In additon, the implemention of ORF in this package, which is in combination with [randomForest](https://cran.r-project.org/web/packages/randomForest/), make it support both incremental learning and batch learning, i.e., construct trees based on randomForest, then update them via ORF. In this way, it will be much faster than before.

Install
=======

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("ZJUguquan/OnlineRandomForest")
```

Quick Start
===========

### 1. Minimal example: incremental learning

``` r
library(OnlineRandomForest)
param <- list('minSamples'= 1, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= dataRange(iris[1:4]))
orf <- ORF$new(param, numTrees = 10)
for (i in 1:150) orf$update(iris[i, 1:4], as.integer(iris[i, 5]))
cat("Mean depth of trees in the forest is:", orf$meanTreeDepth(), "\n")
orf$forest[[2]]$draw()
```

    ## Mean depth of trees in the forest is: 3

    ## Root X4 < 1.21 
    ## |----L: X3 < 2.38 
    ##      |----L: Leaf 1 
    ##      |----R: Leaf 2 
    ## |----R: X4 < 2.15 
    ##      |----L: X1 < 4.92 
    ##           |----L: Leaf 3 
    ##           |----R: Leaf 3 
    ##      |----R: Leaf 3

### 2. Classifaction example

``` r
library(OnlineRandomForest)

# data preparation
dat <- iris; dat[,5] <- as.integer(dat[,5])
x.rng <- dataRange(dat[1:4])
param <- list('minSamples'= 2, 'minGain'= 0.2, 'numClasses'= 3, 'x.rng'= x.rng)
ind.gen <- sample(1:150,30) # for generate ORF
ind.updt <- sample(setdiff(1:150, ind.gen), 100) # for uodate ORF
ind.test <- setdiff(setdiff(1:150, ind.gen), ind.updt) # for test

# construct ORF and update
rf <- randomForest::randomForest(factor(Species) ~ ., data = dat[ind.gen, ], maxnodes = 2, ntree = 100)
orf <- ORF$new(param)
orf$generateForest(rf, df.train = dat[ind.gen, ], y.col = "Species")
cat("Mean size of trees in the forest is:", orf$meanTreeSize(), "\n")
```

    ## Mean size of trees in the forest is: 3

``` r
for (i in ind.updt) {
  orf$update(dat[i, 1:4], dat[i, 5])
}
cat("After update, mean size of trees in the forest is:", orf$meanTreeSize(), "\n")
```

    ## After update, mean size of trees in the forest is: 11.9

``` r
# predict
orf$confusionMatrix(dat[ind.test, 1:4], dat[ind.test, 5], pretty = T)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  20 
    ## 
    ##  
    ##              | actual 
    ##   prediction |         1 |         2 |         3 | Row Total | 
    ## -------------|-----------|-----------|-----------|-----------|
    ##            1 |         4 |         0 |         0 |         4 | 
    ##              |     1.000 |     0.000 |     0.000 |     0.200 | 
    ##              |     1.000 |     0.000 |     0.000 |           | 
    ## -------------|-----------|-----------|-----------|-----------|
    ##            2 |         0 |         9 |         2 |        11 | 
    ##              |     0.000 |     0.818 |     0.182 |     0.550 | 
    ##              |     0.000 |     1.000 |     0.286 |           | 
    ## -------------|-----------|-----------|-----------|-----------|
    ##            3 |         0 |         0 |         5 |         5 | 
    ##              |     0.000 |     0.000 |     1.000 |     0.250 | 
    ##              |     0.000 |     0.000 |     0.714 |           | 
    ## -------------|-----------|-----------|-----------|-----------|
    ## Column Total |         4 |         9 |         7 |        20 | 
    ##              |     0.200 |     0.450 |     0.350 |           | 
    ## -------------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
# compare
table(predict(rf, newdata = dat[ind.test,]) == dat[ind.test, 5])
```

    ## FALSE  TRUE 
    ##     9    11

``` r
table(orf$predicts(X = dat[ind.test,]) == dat[ind.test, 5])
```

    ## FALSE  TRUE 
    ##     2    18

### 3. Regression example

``` r
# data preparation
if(!require(ggplot2)) install.packages("ggplot2")
data("diamonds", package = "ggplot2")
dat <- as.data.frame(diamonds[sample(1:53000,1000), c(1:6,8:10,7)])
for (col in c("cut","color","clarity")) dat[[col]] <- as.integer(dat[[col]]) # Don't forget this
x.rng <- dataRange(dat[1:9])
param <- list('minSamples'= 10, 'minGain'= 1, 'maxDepth' = 10, 'x.rng'= x.rng)
ind.gen <- sample(1:1000, 800)
ind.updt <- sample(setdiff(1:1000, ind.gen), 100)
ind.test <- setdiff(setdiff(1:1000, ind.gen), ind.updt)
```

``` r
# construct ORF 
rf <- randomForest::randomForest(price ~ ., data = dat[ind.gen, ], maxnodes = 20, ntree = 100)
orf <- ORF$new(param)
orf$generateForest(rf, df.train = dat[ind.gen, ], y.col = "price")
orf$meanTreeSize()
```

    ## [1] 39

``` r
# and update
for (i in ind.updt) {
  orf$update(dat[i, 1:9], dat[i, 10])
}
orf$meanTreeSize()
```

    ## [1] 105.7

``` r
# predict and compare
if(!require(Metrics)) install.packages("Metrics")
preds.rf <- predict(rf, newdata = dat[ind.test,])
Metrics::rmse(preds.rf, dat$price[ind.test])
```

    ## [1] 988.8055

``` r
preds <- orf$predicts(dat[ind.test, 1:9])
Metrics::rmse(preds, dat$price[ind.test]) # make progress
```

    ## [1] 869.9613

<br/>

Other use
=========

1.  the **Tree** Class

``` r
ta <- Tree$new("abc", NULL, NULL)
tb <- Tree$new(1, Tree$new(36), Tree$new(3))
tc <- Tree$new(89, tb, ta)
tc$draw()
```

``` r
# update tc
tc$right$updateChildren(Tree$new("666"), Tree$new(999))
tc$right$right$updateChildren(Tree$new("666"), Tree$new(999))
tc$draw()
```

1.  generate a **Online random Tree** from randomForest package, then update it

``` r
# data preparation
library(randomForest)
dat1 <- iris; dat1[,5] <- as.integer(dat1[,5])
rf <- randomForest(factor(Species) ~ ., data = dat1, maxnodes = 3)
treemat1 <- getTree(rf, 1, labelVar=F)
treemat1 <- cbind(treemat1, node.ind = 1:nrow(treemat1))
x.rng1 <- dataRange(dat1[1:4])
param1 <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng1)
ind.gen <- sample(1:150,50) # for generate ORT
ind.updt <- setdiff(1:150, ind.gen) # for update ORT

# origin
ort2 <- ORT$new(param1)
ort2$draw()
```

    ## Root 1 
    ##  Leaf 1 

``` r
# generate a tree
ort2$generateTree(treemat1, df.node = dat1[ind.gen,])
ort2$draw()
```

    ## Root X3 < 2.45 
    ## |----L: Leaf 1 
    ## |----R: X3 < 4.75 
    ##      |----L: Leaf 2 
    ##      |----R: Leaf 3 

``` r
# update this tree
for(i in ind.updt) {
  ort2$update(dat1[i,1:4], dat1[i,5])
}
ort2$draw()
```

    ## Root X3 < 2.45 
    ## |----L: Leaf 1 
    ## |----R: X3 < 4.75 
    ##      |----L: Leaf 2 
    ##      |----R: X4 < 2.19 
    ##           |----L: X2 < 3.68 
    ##                |----L: X1 < 7.12 
    ##                     |----L: X3 < 4.06 
    ##                          |----L: Leaf 1 
    ##                          |----R: Leaf 3 
    ##                     |----R: Leaf 3 
    ##                |----R: Leaf 1 
    ##           |----R: Leaf 3 

More details see in help document
=================================

``` r
help("ORF")
```
