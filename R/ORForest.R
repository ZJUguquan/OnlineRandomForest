
# ORF ------------------------

#' @title Create a Online Random Forest Object 
#' @description ORF is a class of R6.
#' You can use it to create a **random forest** via diffrent ways, which supports incremental learning as well as batch learning.
#' As a matter of fact, the Online Random Forest is made of a list of \code{\link[=ORT]{Online Random Trees}}.
#' @author Quan Gu
#' 
#' @usage ORF$new(param, numTrees = 100)
#' @param param A list which usually has names of \code{minSamples, minGain, numClasses, x.rng, etc.}. 
#' More details show in \code{\link[=ORT]{Online Random Tree}}.
#' @param numTrees A nonnegative integer indicates how many ORT trees are going to build.
#' 
#' @details Online Random Forest was first introduced by \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.150.1671&rep=rep1&type=pdf}{Amir Saffari, etc}. 
#' After that, \href{https://github.com/luiarthur/ORFpy}{Arthur Lui} has implemented the algorithm using Python.
#' Following the paper's advice and Lui's implemention, I refactor the code via R and R6 package. In additon,
#' the implemention of ORF in this package support both incremental learning and batch learning by combining with \code{\link[randomForest]{randomForest}}.
#' For usage, see details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{Online Random Forest}.
#' @format \code{\link{R6Class}} object. 
#' 
#'
#' @section Fields:
#' \describe{
#'   \item{\code{numClasses}}{A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.}
#'   \item{\code{classify}}{TRUE for classification and FALSE for Regression, depending on the value of \code{numClasses}.}
#'   \item{\code{forest}}{A list of ORT trees. More details show in \code{\link[=ORT]{Online Random Tree}}.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{update(x, y)}}{
#'     When a sample comes in, update all ORT trees in forest with the sample's x variables and y value. \cr
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'       \item y - The y value of a sample.
#'     }
#'   }
#'   \item{\code{generateForest(rf, df.train, y.col)}}{
#'     Generate a list of ORT trees, call function \code{\link[=ORT]{ORT$generateTree()}} inside.\cr
#'     \itemize{
#'       \item tree.mat - A tree matrix which can be obtained from \code{randomForest::getTree()}. Node that it must have a column named **node.ind**. See **Examples**. 
#'       \item df.train - The training data frame which has been used to contruct randomForest, i.e., the **data** argument in \code{\link[randomForest]{randomForest}} function. 
#'                        Note that all columns in df.train must be **numeric** or **integer**.
#'       \item y.col - A character indicates which column is y, i.e., the dependent variable. Note that y column must be the last column of df.train.
#'     }
#'   }
#'   \item{\code{predict(x, type = c("class", "prob"))}}{
#'     Predict the corresponding y value of x, using all ORT trees.
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'       \item type - For classification only, **class** means to predict y class for x, and **prob** means to preict probabilities of each class that x belongs to.
#'     }
#'   }
#'   \item{\code{predicts(X, type = c("class", "prob"))}}{
#'     Predict the corresponding y value for a batch of x, using all ORT trees.
#'     \itemize{
#'       \item X - A matrix or a data frame corresponding to a batch of samples' x variables. 
#'       \item type - For classification only, **class** means to predict y class for x, and **prob** means to preict probabilities of each class that x belongs to.
#'     }
#'   }
#'   \item{\code{confusionMatrix(X, y, pretty = FALSE)}}{
#'     Get a confusion matrix about predicted y values and true y values. Only for classification problem.
#'     \itemize{
#'       \item X - A matrix or a data frame corresponding to a batch of samples' x variables. 
#'       \item y - A vector of y values corresponding to a batch of samples.
#'       \item pretty - If TRUE, print a pretty confusion matrix (need \code{gmodels} package). Default FALSE.
#'     }
#'   }
#'   \item{\code{meanTreeSize()}}{Mean size of ORT trees in the forest.}
#'   \item{\code{meanNumLeaves()}}{Mean leaf nodes numbers of ORT trees in the forest.}
#'   \item{\code{meanTreeDepth()}}{Mean depth of ORT trees in the forest.}
#'   \item{\code{sdTreeSize()}}{Standard deviation for size of ORT trees in the forest.}
#'   \item{\code{sdTreeSize()}}{Standard deviation for leaf nodes numbers of ORT trees in the forest.}
#'   \item{\code{sdTreeSize()}}{Standard deviation for depth of ORT trees in the forest.}
#' }
#'
#'
#' @examples
#' # classifaction example
#' dat <- iris; dat[,5] <- as.integer(dat[,5])
#' x.rng <- dataRange(dat[1:4])
#' param <- list('minSamples'= 2, 'minGain'= 0.2, 'numClasses'= 3, 'x.rng'= x.rng)
#' ind.gen <- sample(1:150,30) # for generate ORF
#' ind.updt <- sample(setdiff(1:150, ind.gen), 100) # for uodate ORF
#' ind.test <- setdiff(setdiff(1:150, ind.gen), ind.updt) # for test
#' rf <- randomForest::randomForest(factor(Species) ~ ., data = dat[ind.gen, ], maxnodes = 2, ntree = 100)
#' orf <- ORF$new(param)
#' orf$generateForest(rf, df.train = dat[ind.gen, ], y.col = "Species")
#' orf$meanTreeSize()
#' for (i in ind.updt) {
#'   orf$update(dat[i, 1:4], dat[i, 5])
#' }
#' orf$meanTreeSize()
#' orf$confusionMatrix(dat[ind.test, 1:4], dat[ind.test, 5], pretty = T)
#' # compare
#' table(predict(rf, newdata = dat[ind.test,]) == dat[ind.test, 5])
#' table(orf$predicts(X = dat[ind.test,]) == dat[ind.test, 5])
#'
#'
#' # regression example
#' if(!require(ggplot2)) install.packages("ggplot2")
#' data("diamonds", package = "ggplot2")
#' dat <- as.data.frame(diamonds[sample(1:53000,1000), c(1:6,8:10,7)])
#' for (col in c("cut","color","clarity")) dat[[col]] <- as.integer(dat[[col]]) # Don't forget !
#' x.rng <- dataRange(dat[1:9])
#' param <- list('minSamples'= 10, 'minGain'= 1, 'maxDepth' = 10, 'x.rng'= x.rng)
#' ind.gen <- sample(1:1000, 800)
#' ind.updt <- sample(setdiff(1:1000, ind.gen), 100)
#' ind.test <- setdiff(setdiff(1:1000, ind.gen), ind.updt)
#' rf <- randomForest::randomForest(price ~ ., data = dat[ind.gen, ], maxnodes = 20, ntree = 100)
#' orf <- ORF$new(param)
#' orf$generateForest(rf, df.train = dat[ind.gen, ], y.col = "price")
#' orf$meanTreeSize()
#' for (i in ind.updt) {
#'   orf$update(dat[i, 1:9], dat[i, 10])
#' }
#' orf$meanTreeSize()
#' # compare
#' if(!require(Metrics)) install.packages("Metrics")
#' preds.rf <- predict(rf, newdata = dat[ind.test,])
#' Metrics::rmse(preds.rf, dat$price[ind.test])
#' preds <- orf$predicts(dat[ind.test, 1:9])
#' Metrics::rmse(preds, dat$price[ind.test]) # make progress
#'
#'
#' @import R6 randomForest
#' @export

ORF <- R6Class(
  classname = "Online Random Forest",
  cloneable = FALSE, portable = FALSE, class = FALSE,
  public = list(
    param = NULL, numClasses = NULL, classify = NULL,
    numTrees = NULL, forest = NULL, # ncores = NULL,
    initialize = function(param, numTrees = 100) {
      self$param <- param
      self$numClasses <- ifelse("numClasses" %in% names(param), param[["numClasses"]], 0)
      self$classify <- self$numClasses > 0
      self$numTrees <- numTrees
      self$forest <- replicate(self$numTrees, ORT$new(param), simplify = F)
    },
    update = function(x, y) {
      # stopifnot(is.numeric(y))
      ncores = 0
      if (ncores <= 1) {
        invisible(lapply(self$forest, function(ort) ort$update(x, y)))
      } else {
        # TODO
        library(parallel)
        ncores <- min(ncores, detectCores())
        cluster <- makePSOCKcluster(ncores)
        on.exit(stopCluster(cluster))
        clusterExport(cluster, c("Tree","Elem","Test","SuffStats","ORT","ORF"))
        clusterExport(cluster, c("x","y"), envir = environment())
        self$forest <- parLapply(cluster, self$forest, function(ort){
          ort$update(x, y)
          ort
        })
      }
    },
    generateForest = function(rf, df.train, y.col) {
      # df.train: the training data.frame which was used to contruct rf
      if (!requireNamespace("randomForest", quietly = T))
        stop("You need install.packages('randomForest') first.")
      if (!identical(y.col, names(df.train)[ncol(df.train)]))
        stop("y.col must be the last column of df.train !")
      if(self$classify) {
        if (length(unique(.subset2(df.train, y.col))) > self$numClasses)
            stop("Param numClasses is wrong !")
      }
      if (!("randomForest" %in% class(rf)))
        stop("rf must be an object constructed by `randomForest::randomForest()` !")
      if (rf$forest$ntree != self$numTrees)
        return("The `ntree` parameter in `randomForest::randomForest()` must equal to self$numTrees !")
      ncore = 0
      if (ncore <= 1) {
        invisible(lapply(seq_len(self$numTrees), function(i) {
          tree.mat <- randomForest::getTree(rf, i, labelVar = F)
          tree.mat <- cbind(tree.mat, node.ind = 1:nrow(tree.mat))
          self$forest[[i]]$generateTree(tree.mat, df.node = df.train)
        }))
      } else {
        # TODO
        library(parallel)
        ncores <- 4 #min(ncores, detectCores())
        cluster <- makePSOCKcluster(ncores)
        on.exit(stopCluster(cluster))
        clusterExport(cluster, c("Tree","Elem","Test","SuffStats","ORT","ORF"))
        clusterExport(cluster, c("rf","df.train"), envir = environment())
        self$forest <- parLapply(cluster, seq_len(self$numTrees), function(i){
          tree.mat <- randomForest::getTree(rf, i, labelVar = F)
          tree.mat <- cbind(tree.mat, node.ind = 1:nrow(tree.mat))
          ort <- self$forest[[i]]
          ort$generateTree(tree.mat, df.node = df.train)
          ort
        })
      }
    },
    predict = function(x, type = c("class", "prob"), check.args = FALSE) {
      if (check.args) {
        stopifnot(is.numeric(x))
        type <- match.arg(type)
      } else {
        type <- type[1]
      }
      preds <- unlist(sapply(self$forest, function(ort) ort$predict(x), USE.NAMES = F))
      if (self$classify) {
        pred.counts <- table(preds)
        if (type == "prob") {
          pred.probs <- pred.counts / length(preds)
          if (length(pred.counts) < self$numClasses) {
            add.class <- setdiff(as.character(seq_len(self$numClasses)), names(pred.counts))
            add.probs <- rep(0, length(add.class))
            names(add.probs) <- add.class
            pred.probs <- c(pred.probs, add.probs)
            pred.probs <- pred.probs[order(names(pred.probs))]
          }
          return(pred.probs)
        } else {
          return(as.integer(names(pred.counts)[which.max(pred.counts)]))
        }
      } else {
        return(mean(preds))
      }
    },
    predicts = function(X, type = c("class", "prob")) {
      stopifnot(is.matrix(X) | is.data.frame(X))
      type <- match.arg(type)
      if ((self$classify) & (type == "prob")) {
        return(t(apply(X, 1, self$predict, type = type)))
      } else {
        return(apply(X, 1, self$predict, type = type))
      }
    },
    predStat = function() { "TODO Later" },
    meanTreeSize = function() {
      mean(sapply(self$forest, function(ort) ort$size()))
    },
    meanNumLeaves = function() {
      mean(sapply(self$forest, function(ort) ort$numLeaves()))
    },
    meanTreeDepth = function() {
      mean(sapply(self$forest, function(ort) ort$depth()))
    },
    sdTreeSize = function() {
      sd(sapply(self$forest, function(ort) ort$size()))
    },
    sdNumLeaves = function() {
      sd(sapply(self$forest, function(ort) ort$numLeaves()))
    },
    sdTreeDepth = function() {
      sd(sapply(self$forest, function(ort) ort$depth()))
    },
    confusionMatrix = function(X, y, pretty = FALSE) {
      stopifnot(is.matrix(X) | is.data.frame(X))
      if (!self$classify)
        return("Confusion matrices can only be obtained for classification problem.")
      preds <- self$predicts(X)
      if (!pretty) {
        return(table(pred = preds, y = y))
      } else {
        if (!requireNamespace("gmodels", quietly = T)) {
          return("You need install.packages('gmodels') first.")
        } else {
          return(
            gmodels::CrossTable(preds, y, prop.chisq = FALSE, prop.t = FALSE,
                                dnn = c('prediction', 'actual'))
          )
        }
      }
    }
  )
)







# ------------

# TODO list
# 1. node.counts
# 2. OOBE, rpart to ORT
# 3. parallel
# 4. prob √
# 
# 