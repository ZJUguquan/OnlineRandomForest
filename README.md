Introduction
------------

Online Random Forest was first introduced by [Amir Saffari, etc](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.150.1671&rep=rep1&type=pdf). After that, [Arthur Lui](https://github.com/luiarthur/ORFpy) has implemented the algorithm using Python. Following the paper's advice and Lui's implemention, I refactor the code via R and R6 package. In additon, the implemention of ORF in this package, which is in combination with [randomForest](https://cran.r-project.org/web/packages/randomForest/), make it support both incremental learning and batch learning.

Install
-------

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("ZJUguquan/OnlineRandomForest")
```

Quick Start
-----------

TODO
