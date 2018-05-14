
# SuffStats -----------------------

#' @title Create a SuffStats Object 
#' @description SuffStats is a class of R6. It has provide some important statistics(fields or functions) inside the element of every (leaf) node in an \code{\link{ORT}} object.  
#' @author Quan Gu
#' 
#' @usage SuffStats$new(numClasses = 0)
#' @param numClasses A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.  
#' 
#' @details Note that SuffStats may only be seen in leaf nodes of an ORT tree. See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{SuffStats}.
#' @format \code{\link{R6Class}} object.
#' 
#' @import R6
#' @export
#' 
#' @examples
#' # classification
#' sa <- SuffStats$new(numClasses = 3)
#' sa$update(2);sa$update(1);sa$update(2)
#' sa$pred()
#' sa$node.counts
#' sa$node.n
#' sa$impurity()
#' 
#' # Regression
#' sb <- SuffStats$new(numClasses = 0)
#' sb$update(1);sb$update(2);sb$update(3)
#' sb$pred()
#' sb$node.sum;sb$node.square.sum
#' sb$impurity()
#' 
#' @section Fields:
#' \describe{
#'   \item{\code{node.n}}{Number of samples under current node. Default 0.}
#'   \item{\code{classify}}{TRUE for classification and FALSE for Regression.}
#'   \item{\code{node.counts}}{If classification, counts of each y value in current node.}
#'   \item{\code{node.sum}}{Sum of the y value in current node.}
#'   \item{\code{node.square.sum}}{Sum of the y's square in current node.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{update(y)}}{
#'     When a sample comes in current node, update ORT with the sample's y value. \cr
#'     \itemize{
#'       \item y - The y value of a sample.
#'     }
#'   }
#'   \item{\code{reset()}}{Reset all fields to NULL. Currently not used.}
#'   \item{\code{isLeaf()}}{TRUE if current node is a leaf node otherwise FALSE.}
#'   \item{\code{pred()}}{Return the prediction of current leaf node. Will return an **integer** for classification or an **numeric** for regression.}
#'   \item{\code{impurity()}}{Return the impurity of current node. Computed via information entropy.}
#' }

SuffStats <- R6Class(
  classname = "SuffStats",
  public = list(
    eps = 1e-10, node.n = 0, classify = NULL,
    node.counts = NULL, node.sum = NULL, node.square.sum = NULL,
    initialize = function(numClasses = 0) {
      self$classify = numClasses > 0
      if (numClasses > 0) {
        self$node.counts <- rep(0, numClasses)
      } else {
        self$node.sum <- 0.0
        self$node.square.sum <- 0.0
      }
    },
    update = function(y) {
      self$node.n <- self$node.n + 1
      if (self$classify) {
        self$node.counts[y] <- self$node.counts[y] + 1 # need modify
      } else {
        self$node.sum <- self$node.sum + y
        self$node.square.sum <- self$node.square.sum + y^2
      }
    },
    reset = function() {
      self$eps <- self$n <- self$classify <- NULL
      self$node.counts <- self$node.sum <- self$node.square.sum <- NULL
    },
    pred = function() {
      if (self$classify) {
        # what if same frequency?
        # name of counts?
        which.max(self$node.counts)
      } else {
        unlist(self$node.sum / (self$node.n + self$eps))
      }
    },
    impurity = function() {
      n <- self$node.n + self$eps
      if (self$classify) {
        prop <- self$node.counts / n
        sum(-prop * log(prop + self$eps))
      } else {
        pred <- self$pred()
        sqrt(self$node.square.sum / n - pred^2)
      }
    }
  )
)



# Test -------------------------

#' @title Create a Candidate Split Object 
#' @description Test is a class of R6. It is the candicate split of a node. Named by *Amir Saffari* 's paper. 
#' @author Quan Gu
#' 
#' @usage Test$new(xvar.index, xvar.value, numClasses = 0)
#' @param xvar.index A integer indicates which x variable is used to split.  
#' @param xvar.value A numeric indicates what value of the chosen x variable is used to split. Means: \code{x[xvar.index] < xvar.value}.
#' @param numClasses A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.
#' 
#' @details Note that Test may only be seen in leaf nodes of an ORT tree. See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{Candidate Split}.
#' @format \code{\link{R6Class}} object.
#' 
#' @import R6
#' @export
#' 
#' @examples
#' t1 <- Test$new(3, 2.2, 0)
#' t1$update(c(0,0,2,4), 1.2)
#' t1$update(c(0,0,1,4), 1.4)
#' t1$update(c(1,1,3,3), 2.7)
#' t1$statsL$pred()
#' t1$statsR$pred()
#'
#' @section Fields:
#' \describe{
#'   \item{\code{statsL}}{A SuffStats object in left hand of a split.}
#'   \item{\code{statsR}}{A SuffStats object in right hand of a split.}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{update(x, y)}}{
#'     When a sample comes in current node, update ORT with the sample's x variables and y value. \cr
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'       \item y - The y value of a sample.
#'     }
#'   }
#' }
   
Test <- R6Class(
  classname = "Candidate Split",
  public = list(
    classify = NULL, xvar.index = NULL, xvar.value = NULL,
    statsL = NULL, statsR = NULL,
    initialize = function(xvar.index, xvar.value, numClasses = 0) {
      self$classify <- numClasses > 0
      self$xvar.index <- xvar.index
      self$xvar.value <- xvar.value
      self$statsL <- SuffStats$new(numClasses = numClasses)
      self$statsR <- SuffStats$new(numClasses = numClasses)
    },
    update = function(x, y) {
      if (x[self$xvar.index] < self$xvar.value) {
        self$statsL$update(y)
      } else {
        self$statsR$update(y)
      }
    }
  )
)



# Elem -------------------------

#' @title Create a Node Element Object 
#' @description Elem is a class of R6. It is the element of a node in a Tree or ORT object, storing some important information of the node/Tree.
#' @author Quan Gu
#' 
#' @usage Elem$new(param, splitInd = -1, splitVal = 0, numSamplesSeen = 0)
#' @param param A list which usually has names of \code{minSamples, minGain, numClasses, x.rng, etc.}
#' @param splitInd A integer indicates which x variable is used to split on current node. Default NULL.
#' @param splitVal A numeric indicates what value of the chosen x variable is used to split on current node. Means: \code{x[splitInd] < splitVal}. Default NULL.
#' @param numSamplesSeen A integer indicates how many samples have come in current node for updating the ORT tree. Default 0.
#' 
#' @details See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{Tree Element}.
#' @format \code{\link{R6Class}} object.
#' 
#' @import R6
#' @export
#' 
#' @examples
#' x.rng <- data.frame(min = c(1,3,5,7), max = c(2,4,6,8), row.names = paste0("x",1:4))
#' param <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng)
#' elm <- Elem$new(param)
#' length(elm$tests)
#' elm$tests[c(1,8)]
#' elm$updateSplit(3, 5.2)
#' elm$toString()
#' elm$update(c(1.1, 3.2, 5.3, 7.4), 1)
#'
#' @section Fields:
#' \describe{
#'   \item{\code{x.rng}}{
#'     A data frame which indicates the range of every x variable in training data.
#'     It must be a shape of \code{n*2} which n is the number of x variables, i.e. \code{x.dim}.
#'     And the first collumn must be the minimal values of x and the second as maximum.
#'     You can generate it via \code{OnlineRandomForest::dataRange()} for convenience.
#'   }
#'   \item{\code{x.dim}}{Number of x variables.}
#'   \item{\code{tests}}{A list of \code{SuffStats}. Candicate splits of current node.}
#'   \item{\code{numTests}}{A part of \code{param} indicates the number of \code{SuffStats} in tests}
#'   \item{\code{stats}}{A \code{SuffStats} object. The real splits of current node.}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{toString()}}{Print elem itself or the split of condition on current node.}
#'   \item{\code{pred()}}{Return the prediction of current leaf node. Will return an **integer** for classification or an **numeric** for regression.}
#'   \item{\code{update(x, y)}}{
#'     When a sample comes in current node, update ORT with the sample's x variables and y value. \cr
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'       \item y - The y value of a sample.
#'     }
#'   }
#'   \item{\code{updateSplit(xvar.index, xvar.value)}}{
#'     Replace the fields **splitInd** and **splitVal** of current node with xvar.index and xvar.value.
#'   }
#'   \item{...}{Other functions.}
#' }

Elem <- R6Class(
  classname = "Node Element",
  public = list(
    x.rng = NULL, x.dim = NULL, splitInd = NULL, splitVal = NULL,
    numClasses = NULL, numTests = NULL, numSamplesSeen = NULL,
    stats = NULL, tests = NULL,
    initialize = function(param, splitInd = -1, splitVal = 0, numSamplesSeen = 0) {
      stopifnot(is.list(param))
      stopifnot(is.matrix(param[["x.rng"]]) | is.data.frame(param[["x.rng"]]))
      self$x.rng <- param[["x.rng"]] # clean or not
      self$x.dim <- nrow(param[["x.rng"]]) # clean or not
      self$numClasses <- ifelse("numClasses" %in% names(param), param[["numClasses"]], 0) # clean or not
      self$numTests <- ifelse("numTests" %in% names(param), param[["numTests"]], 10) # clean or not
      self$splitInd <- splitInd
      self$splitVal <- splitVal
      self$numSamplesSeen <- numSamplesSeen
      self$stats <- SuffStats$new(numClasses = self$numClasses)
      # self$tests <- replicate(self$numTests, self$generateTest(), simplify = F) # loop
      self$tests <- self$generateTests() # 1/3 faster
    },
    reset = function() {
      self$stats <- self$tests <- NULL
      self$x.rng <- self$x.dim <- NULL
      self$numClasses <- self$numTests <- NULL
    },
    generateTest = function() {
      xvar.index <- sample.int(self$x.dim, 1)
      xvar.value <- runif(1, self$x.rng[xvar.index,1], self$x.rng[xvar.index,2])
      Test$new(xvar.index, xvar.value, self$numClasses)
    },
    generateTests = function() {
      xvar.inds <- sample.int(self$x.dim, self$numTests, replace = T)
      xvar.vals <- runif(self$numTests, self$x.rng[xvar.inds,1], self$x.rng[xvar.inds,2])
      lapply(seq_len(self$numTests), function(i) Test$new(xvar.inds[i], xvar.vals[i], self$numClasses))
    },
    toString = function() {
      ifelse(identical(self$splitInd, -1),
             as.character(round(self$pred(), 4)),
             paste0("X", self$splitInd, " < ", round(self$splitVal, 2)))
    },
    pred = function() self$stats$pred(),
    update = function(x, y) {
      self$stats$update(y)
      self$numSamplesSeen <- self$numSamplesSeen + 1
      invisible(lapply(self$tests, function(test) test$update(x, y)))
    },
    updateSplit = function(xvar.index, xvar.value) {
      self$splitInd <- xvar.index
      self$splitVal <- xvar.value
    }
  )
)



# dataRange -------------------------

#' @title dataRange function
#' @description Return the range of X for each variable.
#' @author Quan Gu
#' 
#' @usage dataRange(X)
#' @param X A matrix or a data frame of training data **without** y column.
#' 
#' @details Return the range of X for each variable, which is a part of \code{param}. Can be used for passing \code{param} to \code{Elem, ORT or ORF}.
#' @return A data frame with two columns \code{min max} contains the range of each x variable.
#' 
#' @examples
#' dataRange(iris[, 1:4])
#' dataRange(mtcars)

dataRange <- function(X) {
  stopifnot(is.matrix(X) | is.data.frame(X))
  data.frame(min = apply(X, 2, min),
             max = apply(X, 2, max),
             row.names = paste0("X", 1:ncol(X)))
}




# ORT -----------------------

#' @title Create a Online Random Tree Object 
#' @description ORT is a class of R6 and it inherits the \code{\link{Tree}} class.
#' You can use it to create a **decision tree** via diffrent ways, which supports incremental learning as well as batch learning.
#' @author Quan Gu
#' 
#' @usage ORT$new(param)
#' @param param A list which usually has names of \code{minSamples, minGain, numClasses, x.rng, etc.}. 
#' More details show in **Fields**.
#' 
#' @details See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{Online Random Tree}.
#' @format \code{\link{R6Class}} object. 
#' 
#' @import R6
#' @export
#' 
#' @examples
#' if(!require(randomForest)) install.packages("randomForest")
#' library(randomForest)
#' 
#' # classifaction example
#' dat1 <- iris; dat1[,5] <- as.integer(dat1[,5])
#' rf <- randomForest(factor(Species) ~ ., data = dat1)
#' treemat1 <- getTree(rf, 1, labelVar=F)
#' treemat1 <- cbind(treemat1, node.ind = 1:nrow(treemat1))
#' x.rng1 <- data.frame(min = apply(dat1[1:4], 2, min), 
#'                      max = apply(dat1[1:4], 2, max), 
#'                      row.names = paste0("X",1:4))
#' param1 <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng1)
#' ort1 <- ORT$new(param1)
#' ort1$generateTree(treemat1, df.node = dat1) # 23ms, 838KB
#' ort1$draw()
#' ort1$left$elem$tests[[1]]$statsL
#' sapply(1:150, function(i) ort1$predict(dat1[i,1:4]))
#' 
#' # first generate, then update
#' ind.gen <- sample(1:150,50) # for generate ORT
#' ind.updt <- setdiff(1:150, ind.gen) # for update ORT
#' rf2 <- randomForest(factor(Species) ~ ., data = dat1[ind.gen,])
#' treemat2 <- getTree(rf2, 22, labelVar=F)
#' treemat2 <- cbind(treemat2, node.ind = 1:nrow(treemat2))
#' ort2 <- ORT$new(param1)
#' ort2$draw()
#' ort2$generateTree(treemat2, df.node = dat1[ind.gen,])
#' ort2$draw()
#' for(i in ind.updt) {
#'   ort2$update(dat1[i,1:4], dat1[i,5])
#' }
#' ort2$draw()
#' 
#' 
#' # regression example
#' if(!require(ggplot2)) install.packages("ggplot2")
#' data("diamonds", package = "ggplot2")
#' dat3 <- as.data.frame(diamonds[sample(1:53000,1000), c(1:6,8:10,7)])
#' for (col in c("cut","color","clarity")) dat3[[col]] <- as.integer(dat3[[col]])
#' x.rng3 <- data.frame(min = apply(dat3[1:9], 2, min),
#'                      max = apply(dat3[1:9], 2, max),
#'                      row.names = paste0("X", 1:9))
#' param3 <- list('minSamples'= 10, 'minGain'= 1, 'maxDepth' = 10, 'x.rng'= x.rng3)
#' ind.gen3 <- sample(1:1000,500)
#' ind.updt3 <- setdiff(1:1000, ind.gen3)
#' rf3 <- randomForest(price ~ ., data=dat3[ind.gen3,], maxnodes=20)
#' treemat3 <- getTree(rf3, 33, labelVar=F)
#' treemat3 <- cbind(treemat3, node.ind = 1:nrow(treemat3))
#' 
#' ort3 <- ORT$new(param3)
#' ort3$generateTree(treemat3, df.node = dat3[ind.gen3,])
#' ort3$size()
#' for (i in ind.updt3) {
#'   ort3$update(dat3[i,1:9], dat3[i,10])
#' }
#' ort3$size()
#' 
#' @section Fields:
#' \describe{
#'   \item{\code{age}}{How many times has the loop go through inside the \code{update()} function.}
#'   \item{\code{minSamples}}{A part of \code{param} indicates the minimal samples in a leaf node}
#'   \item{\code{minGain}}{A part of \code{param} indicates minimal entropy gain when split a node.}
#'   \item{\code{numTests}}{A part of \code{param} indicates the number of \code{SuffStats} in tests. Default 10 if not set.}
#'   \item{\code{maxDepth}}{A part of \code{param} indicates max depth of an ORT tree. Default 10 if not set.}
#'   \item{\code{numClasses}}{A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.}
#'   \item{\code{classValues}}{All diffrent possible values of y if classification. Default NULL if not set.}
#'   \item{\code{x.rng}}{
#'     A data frame which indicates the range of every x variable in training data.
#'     It must be a shape of \code{n*2} which n is the number of x variables, i.e. \code{x.dim}.
#'     And the first collumn must be the minimal values of x and the second as maximum.
#'     You can generate it via \code{OnlineRandomForest::dataRange()} for convenience.
#'   }
#'   \item{\code{...}}{Other fields can be seen in \code{\link{Tree}}.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{findLeaf(x, tree, depth = 0)}}{
#'     Find the leaf node where x is located. Return a list, including node and its depth.
#'     \itemize{
#'       \item x - A sample of x. \cr
#'       \item tree - An ORT tree or node.
#'     }
#'   }
#'   \item{\code{gains(elem)}}{
#'     Compute the entropy gain on all tests of the elem.
#'     \itemize{
#'       \item elem - An \code{Elem} object.
#'     }
#'   }
#'   \item{\code{update(x, y)}}{
#'     When a sample comes in current node, update ORT with the sample's x variables and y value. \cr
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'       \item y - The y value of a sample.
#'     }
#'   }
#'   \item{\code{generateTree(tree.mat, df.node, node.ind = 1)}}{
#'     Generate a Tree from a tree matrix which just likes the result of \code{randomForest::getTree()}.\cr
#'     \itemize{
#'       \item tree.mat - A tree matrix which can be obtained from \code{randomForest::getTree()}. Node that it must have a column named **node.ind**. See **Examples**. \cr
#'       \item node.ind - The index of the current node in Tree. Default \code{1} for the root node. For most purposes, don't need to change it.
#'       \item df.node - The training data frame which has been used to contruct random forest.,
#'        i.e., the **data** argument in \code{\link[randomForest]{randomForest}} function.
#'     }
#'   }
#'   \item{\code{predict(x)}}{
#'     Predict the corresponding y value of x.
#'     \itemize{
#'       \item x - The x variables of a sample. Note it is an numeric vector other than a scalar.
#'     }
#'   }
#'   \item{\code{draw()}}{Draw the Tree.}
#'   \item{\code{...}}{Other methods can be seen in \code{\link{Tree}}.}
#' }

ORT <- R6Class(
  classname = "Online Random Tree",
  inherit = Tree,
  public = list(
    param = NULL, age = 0, minSamples = NULL, minGain = NULL,
    x.rng = NULL, gamma = NULL, numTests = NULL, maxDepth = NULL,
    numClasses = NULL, classValues = NULL,
    initialize = function(param) {
      self$param <- param # only leaf node need
      self$age <- 0
      self$minSamples <- param[["minSamples"]]
      self$minGain <- param[["minGain"]]
      self$elem<- Elem$new(param = param) # inherit
      param.names <- names(param)
      # self$gamma <- ifelse("gamma" %in% param.names, param[["gamma"]], 0) # TODO
      self$numTests <- ifelse("numTests" %in% param.names, param[["numTests"]], 10)
      self$maxDepth <- ifelse("maxDepth" %in% param.names, param[["maxDepth"]], 10)
      self$numClasses <- ifelse("numClasses" %in% param.names, param[["numClasses"]], 0)
    },
    findLeaf = function(x, tree, depth = 0) {
      if (tree$isLeaf()) {
        return(list(j = tree, depth = depth))
      } else {
        x.ind <- tree$elem$splitInd
        x.val <- tree$elem$splitVal
        if (x[x.ind] < x.val) {
          return(self$findLeaf(x, tree$left, depth+1))
        } else {
          return(self$findLeaf(x, tree$right, depth+1))
        }
      }
    },
    gains = function(elem) {
      tests <- elem$tests
      on.exit(rm(tests))
      gain <- function(test) {
        statsL <- test$statsL; statsR <- test$statsR
        nL <- statsL$node.n; nR <- statsR$node.n
        n <- nL + nR + 1e-9
        lossL <- ifelse(nL==0, 0, statsL$impurity())
        lossR <- ifelse(nR==0, 0, statsR$impurity())
        g <- elem$stats$impurity() - (nL/n)*lossL - (nR/n)*lossR
        max(g, 0)
      }
      sapply(tests, gain)
    },
    update = function(x, y) {
      k = rpois(1, lambda = 1)
      if (k == 0) {
        private$updateOOBE__(x, y)
      } else {
        for (u in seq_len(k)) {
          self$age <- self$age + 1
          current.leaf <- self$findLeaf(x, self)
          j <- current.leaf$j # named by paper, atually the leaf itself
          depth <- current.leaf$depth
          j$elem$update(x, y)
          if (j$elem$stats$node.n > self$minSamples & depth < self$maxDepth) {
            g <- self$gains(j$elem)
            if (any(g > self$minGain)) {
              bestTest <- j$elem$tests[[which.max(g)]] # fuck, j$elem not self$elem !!
              j$elem$updateSplit(bestTest$xvar.index, bestTest$xvar.value)
              j$updateChildren(l = Tree$new(Elem$new(self$param)),
                               r = Tree$new(Elem$new(self$param)))
              j$left$elem$stats <- bestTest$statsL
              j$right$elem$stats <- bestTest$statsR
              j$elem$reset() # for saving memory
            }
          }
        }
      }
    },
    generateTree = function(tree.mat, df.node, node.ind = 1) {
      super$generateTree(tree.mat, node.ind)
      private$updateLeafElem(df.node, self, check.y = T)
    },
    predict = function(x) {
      current.leaf <- self$findLeaf(x, self)
      # current.leaf$j$elem$stats$pred()
      current.leaf$j$elem$pred()
    }
  ),
  private = list(
    updateLeafElem = function(df.node, tree, check.y = T) {
      # check whether y.col is the last col of df.node, only once
      if (check.y) {
        y.col.ind <- ncol(df.node)
        if (self$numClasses > 0) {
          stopifnot(is.integer(.subset2(df.node, y.col.ind)))
          stopifnot(min(.subset2(df.node, y.col.ind)) == 1) # TODO
          self$classValues <- sort(unique(.subset2(df.node, y.col.ind)))
        } else {
          stopifnot(is.numeric(.subset2(df.node, y.col.ind)))
        }
      }
      # recursively update leaf node
      if (tree$isLeaf()) {
        nrow.df <- nrow(df.node)
        if (nrow.df == 0) {
          tree$elem <- Elem$new(param = self$param)
          return(invisible(NULL))
        }
        y.col.ind <- ncol(df.node)
        tree$elem <- Elem$new(param = self$param)
        tree$elem$stats$node.n <- nrow.df

        # update stats and tests in tree$elem
        if (self$numClasses > 0) {
          tree$elem$stats$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind), self$classValues)
          invisible(lapply(tree$elem$tests, function(test) {
            test.left.inds <- (.subset2(df.node, test$xvar.index) < test$xvar.value)
            test$statsL$node.n <- sum(test.left.inds)
            if (test$statsL$node.n > 0) {
              test$statsL$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind)[test.left.inds], self$classValues)
            } else {
              test$statsL$node.counts <- rep(0, self$numClasses)
            }
            # test$statsR$node.n <- sum(!test.left.inds)
            test$statsR$node.n <- nrow.df - test$statsL$node.n
            if (test$statsR$node.n > 0) {
              test$statsR$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind)[!test.left.inds], self$classValues)
            } else {
              test$statsR$node.counts <- rep(0, self$numClasses)
            }
          }))
        } else {
          tree$elem$stats$node.sum <- sum(.subset2(df.node, y.col.ind))
          tree$elem$stats$node.square.sum <- sum(.subset2(df.node, y.col.ind) ^ 2)
          invisible(lapply(tree$elem$tests, function(test) {
            test.left.inds <- (.subset2(df.node, test$xvar.index) < test$xvar.value)
            # maybe need if...else...
            test$statsL$node.n <- sum(test.left.inds)
            test$statsL$node.sum <- sum(.subset2(df.node, y.col.ind)[test.left.inds])
            test$statsL$node.square.sum <- sum(.subset2(df.node, y.col.ind)[test.left.inds] ^ 2)
            # test$statsR$node.n <- sum(!test.left.inds)
            test$statsR$node.n <- nrow.df - test$statsL$node.n
            test$statsR$node.sum <- sum(.subset2(df.node, y.col.ind)[!test.left.inds])
            test$statsR$node.square.sum <- sum(.subset2(df.node, y.col.ind)[!test.left.inds] ^ 2)
          }))
        }

      } else {
        left.inds <- (.subset2(df.node, tree$elem$splitInd) < tree$elem$splitVal)
        private$updateLeafElem(df.node[left.inds, ], tree$left, check.y = F)
        private$updateLeafElem(df.node[!left.inds, ], tree$right, check.y = F)
      }
    },
    valueCounts = function(y, cls.values) {
      # cls.values: all the possible class values of y.
      table(factor(y, levels = cls.values))
    },
    poisson__ =  function(lam = 1){
      l = exp(-lam)
      loop <- function(k,p) ifelse(p>l, loop(k+1, p*runif(1)), k-1)
      loop(0,1)
    },
    updateOOBE__ = function(x, y) {} # TODO
  )
)


