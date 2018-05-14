# Tree ------------------------

#' @title Create a Tree Object 
#' @description Tree is a class of R6. You can use it to create a binary tree via many ways.
#' @author Quan Gu
#' 
#' @usage Tree$new(elem = NULL, left = NULL, right = NULL, node.ind = 1)
#' @param elem The element in the tree. More details show in \code{help(Elem)}. Default NULL.
#' @param left The left child of the Tree. Also a Tree oject. Default NULL.
#' @param right The right child of the Tree. Also a Tree oject. Default NULL.
#' @param node.ind The index of the current node in Tree. Default \code{1} for the root node.
#' 
#' @details See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of Tree.
#' @format \code{\link{R6Class}} object.
#' 
#' @import R6
#' @export
#' 
#' @examples
#' ta <- Tree$new("abc", NULL, NULL)
#' tb <- Tree$new(1, Tree$new(36), Tree$new(3))
#' tc <- Tree$new(89, tb, ta)
#' td <- Tree$new("guquan", tc, tb)
#' 
#' tb$draw()
#' tb$size()
#' tb$depth()
#' tc$draw()
#' tc$depth()
#' tc$right$updateChildren(Tree$new("666"), Tree$new(999))
#' tc$right$right$updateChildren(Tree$new("666"), Tree$new(999))
#' tc$draw()
#' td$draw()
#' 
#' # generate a Tree from randomForest
#' if (!require(randomForest)) install.packages("randomForest")
#' library(randomForest)
#' dat <- iris; dat[,5] <- as.integer(dat[,5])
#' rf <- randomForest(factor(Species) ~ ., data=dat)
#' treemat <- getTree(rf, 1, labelVar = F)
#' treemat <- cbind(treemat, node.ind = 1:nrow(treemat))
#' tree <- Tree$new()
#' tree$generateTree(treemat)
#' tree$draw()
#'
#' @section Fields:
#' \describe{See **Arguments**.}
#'
#' @section Methods:
#' \describe{
#'   \item{\code{generateTree(tree.mat, node.ind = 1)}}{
#'     Generate a Tree from a tree matrix which just likes the result of \code{randomForest::getTree()}.\cr
#'     \itemize{
#'       \item tree.mat - A tree matrix which can be obtained from \code{randomForest::getTree()}. Node that it must have a column named **node.ind**. See **Examples**. \cr
#'       \item node.ind - The index of the current node in Tree. Default \code{1} for the root node. For most purposes, don't need to change it.
#'     }
#'   }
#'   \item{\code{updateChildren(l, r)}}{
#'     \itemize{
#'       Update childre of the current node.
#'       \item l - The left child. \cr
#'       \item r - The right child.
#'     }
#'   }
#'   \item{\code{isLeaf()}}{TRUE if current node is a leaf node otherwise FALSE.}
#'   \item{\code{size()}}{Return size of Tree. For each node, size + 1.}
#'   \item{\code{numLeaves()}}{Return how many leaf nodes in Tree.}
#'   \item{\code{depth()}}{Return max depth of Tree. If only a root node, depth is 0.}
#'   \item{\code{draw()}}{Draw the Tree.}
#' }

Tree <- R6Class(
  classname = "Tree",
  public = list(
    elem = NULL, left = NULL, right = NULL, node.ind = NULL,
    initialize = function(elem = NULL, left = NULL, right = NULL, node.ind = 1) {
      stopifnot((is.null(left) & is.null(right)) | 
                 sum(c(class(left), class(right)) == "Tree") == 2)
      self$elem <- elem
      self$left <- left
      self$right <- right
      self$node.ind <- node.ind
    },
    generateTree = function(tree.mat, node.ind = 1) {
      row <- tree.mat[node.ind, ]
      if (row[["status"]] != -1) {
        self$elem <- list(splitInd = row[["split var"]], splitVal = row[["split point"]])
        self$elem$toString <- function() {
          paste0("X", self$elem$splitInd, " < ", self$elem$splitVal)
          # paste0("X", eval(self$elem$splitInd), " < ", eval(self$elem$splitVal))
          # paste0("X", eval(parse(text = "self$elem$splitInd")), " < ", eval(parse(text = "self$elem$splitVal")))
        }
        self$left <- Tree$new(node.ind = row[["left daughter"]])
        self$left$generateTree(tree.mat, row[["left daughter"]])
        self$right <- Tree$new(node.ind = row[["right daughter"]])
        self$right$generateTree(tree.mat, row[["right daughter"]])
      } else {
        self$elem <- row[["prediction"]]
      }
    },
    updateChildren = function(l, r) {
      self$left <- l
      self$right <- r
    },
    isLeaf = function() {
      is.null(self$left) & is.null(self$right)
    },
    size = function() {
      if (self$isLeaf()) 1 else { self$left$size() + self$right$size() + 1 }
    },
    numLeaves = function() {
      if (self$isLeaf()) 1 else { self$left$numLeaves() + self$right$numLeaves() }
    },
    depth = function() {
      if (self$isLeaf()) 0 else { max(self$left$depth(), self$right$depth()) + 1 }
    },
    draw = function(depth = 0, side = "none") {
      if ("toString" %in% names(self$elem)) {
        elem <- self$elem$toString()
      } else {
        elem <- self$elem
      }
      if (depth == 0) {
        cat("Note: if condition satisfied, go L(Left), otherwise R(Right).\n\n")
        cat("Root", elem, "\n")
      }
      prefix = paste0(rep("     ", max(depth-1, 0)), collapse = "")
      prefix = paste0(prefix, switch(side, left = "|----L:", right = "|----R:", none = ""))
      if (self$isLeaf()) {
        cat(prefix, "Leaf", elem, "\n")
      } else {
        if (depth != 0) cat(prefix, elem, "\n")
        self$left$draw(depth+1, "left")
        self$right$draw(depth+1, "right")
      }
    }
  )          
)



# SuffStats -----------------------

#' @title Create a SuffStats Object 
#' @description SuffStats is a class of R6. It has provide some important statistics(fields or functions) inside the element of every (leaf) node in an ORT object.  
#' @author Quan Gu
#' 
#' @usage SuffStats$new(numClasses = 0)
#' @param numClasses A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.  
#' 
#' @details Note that SuffStats may only be seen in leaf nodes of an ORT tree. See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of SuffStats.
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
#'   \item{\code{(node.n)}}{Number of samples under current node. Default 0.}
#'   \item{\code{(classify)}}{TRUE for classification and FALSE for Regression.}
#'   \item{\code{(node.counts)}}{If classification, counts of each y value in current node.}
#'   \item{\code{(node.sum)}}{Sum of the y value in current node.}
#'   \item{\code{(node.square.sum)}}{Sum of the y's square in current node.}
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
#'   \item{\code{pred()}}{Return the prediction of current leaf node. Will be an **integer** for classification or an **numeric** for regression.}
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

#' @title Create a Test Object 
#' @description Test is a class of R6. It is the candicate split of a node. Named by *Amir Saffari* 's paper. 
#' @author Quan Gu
#' 
#' @usage Test$new(xvar.index, xvar.value, numClasses = 0)
#' @param xvar.index A integer indicates which x variable is used to split.  
#' @param xvar.value A numeric indicates what value of the chosen x variable is used to split. Means \code{x[xvar.index] < xvar.value}.
#' @param numClasses A nonnegative integer indicates how many classes when solve a classifation problem. Default 0 for regression. If numClasses > 0, then do classifation.
#' 
#' @details Note that Test may only be seen in leaf nodes of an ORT tree. See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of Test.
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
#'   \item{\code{(statsL)}}{A SuffStats object in left hand of a split.}
#'   \item{\code{(statsR)}}{A SuffStats object in right hand of a split.}
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

#' @title Create a Elem Object 
#' @description Elem is a class of R6. It is the element of a node in a Tree or ORT object.
#' @author Quan Gu
#' 
#' @usage Elem$new(param, splitInd = -1, splitVal = 0, numSamplesSeen = 0)
#' @param param A list which usually has names of \code{}
#' @param splitInd The left child of the Tree. Also a Tree oject. Default NULL.
#' @param splitVal The right child of the Tree. Also a Tree oject. Default NULL.
#' @param numSamplesSeen The index of the current node in Tree. Default \code{1} for the root node.
#' 
#' @details See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of Tree.
#' @format \code{\link{R6Class}} object.
#' 
#' @import R6
#' @export
#' 
#' @examples
#' x.rng <- data.frame(min = c(1,3,5,7), max = c(2,4,6,8), row.names = paste0("x",1:4))
#' param <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng)
#' elm1 <- Elem$new(param)
#' length(elm1$tests)
#' elm1$tests[c(1,8)]
#' elm1$updateSplit(3, 5.2)
#' elm1$toString()
#' elm1$update(c(1.1, 3.2, 5.3, 7.4), 1)
#' elm1$update(c(1.11, 3.22, 5.33, 7.44), 1)
#' elm1$update(c(1.9, 3.9, 5.9, 7.9), 3)

Elem <- R6Class(
  classname = "Tree Element",
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






# 
# 
# # ORT --------------------------
# 
# x.rng1 <- data.frame(min = apply(dat1[1:4], 2, min),
#                      max = apply(dat1[1:4], 2, max),
#                      row.names = paste0("X",1:4))
# param1 <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng1)
# 
# 
# if (F) {
# 
# # Tree <- R6Class("ORF.R$Tree")
# 
# ORT <- R6Class(
#   classname = "Online Random Tree",
#   inherit = Tree,
#   public = list(
#     param = NULL, age = 0, minSamples = NULL, minGain = NULL,
#     x.rng = NULL, gamma = NULL, numTests = NULL, numClasses = NULL,
#     maxDepth = NULL, node.ind = NULL,
#     initialize = function(param, node.ind = 1) {
#       self$param <- param # only leaf node need
#       self$age <- 0
#       self$minSamples <- param[["minSamples"]]
#       self$minGain <- param[["minGain"]]
#       self$elem<- Elem$new(param = param) # inherit
#       param.names <- names(param)
#       # self$gamma <- ifelse("gamma" %in% param.names, param[["gamma"]], 0) # TODO
#       self$numTests <- ifelse("numTests" %in% param.names, param[["numTests"]], 10)
#       self$numClasses <- ifelse("numClasses" %in% param.names, param[["numClasses"]], 0)
#       self$maxDepth <- ifelse("maxDepth" %in% param.names, param[["maxDepth"]], 10)
#       self$node.ind <- node.ind
#     },
#     findLeaf = function(x, tree, depth = 0) {
#       if (tree$isLeaf()) {
#         return(list(j = tree, depth = depth))
#       } else {
#         x.ind <- tree$elem$splitInd
#         x.val <- tree$elem$splitVal
#         if (x[x.ind] < x.val) {
#           return(self$findLeaf(x, tree$left, depth+1))
#         } else {
#           return(self$findLeaf(x, tree$right, depth+1))
#         }
#       }
#     },
#     gains = function(elem) {
#       tests <- elem$tests
#       on.exit(rm(tests))
#       gain <- function(test) {
#         statsL <- test$statsL; statsR <- test$statsR
#         nL <- statsL$node.n; nR <- statsR$node.n
#         n <- nL + nR + 1e-9
#         lossL <- ifelse(nL==0, 0, statsL$impurity())
#         lossR <- ifelse(nR==0, 0, statsR$impurity())
#         g <- elem$stats$impurity() - (nL/n)*lossL - (nR/n)*lossR
#         max(g, 0)
#       }
#       sapply(tests, gain)
#     },
#     update = function(x, y) {
#       k = rpois(1, lambda = 1)
#       if (k == 0) {
#         self$updateOOBE__(x, y)
#       } else {
#         for (u in seq_len(k)) {
#           self$age <- self$age + 1
#           current.leaf <- self$findLeaf(x, self)
#           j <- current.leaf$j # named by paper, atually the leaf itself
#           depth <- current.leaf$depth
#           j$elem$update(x, y)
#           if (j$elem$stats$node.n > self$minSamples & depth < self$maxDepth) {
#             g <- self$gains(j$elem)
#             if (any(g > self$minGain)) {
#               bestTest <- j$elem$tests[[which.max(g)]] # fuck, j$elem not self$elem !!
#               j$elem$updateSplit(bestTest$xvar.index, bestTest$xvar.value)
#               j$updateChildren(l = Tree$new(Elem$new(self$param)),
#                                r = Tree$new(Elem$new(self$param)))
#               j$left$elem$stats <- bestTest$statsL
#               j$right$elem$stats <- bestTest$statsR
#               j$elem$reset() # for saving memory
#             }
#           }
#         }
#       }
#     },
#     generateTree = function(tree.mat, node.ind = 1) {
#       row <- tree.mat[node.ind, ]
#       if (row[["status"]] != -1) {
#         self$elem <- list(splitInd = row[["split var"]], splitVal = row[["split point"]])
#         self$elem$toString <- function() {
#           paste0("X", self$elem$splitInd, " < ", self$elem$splitVal)
#         }
#         self$left <- ORT$new(self$param, node.ind = row[["left daughter"]])
#         self$left$generateTree(tree.mat, row[["left daughter"]])
#         self$right <- ORT$new(self$param, node.ind = row[["right daughter"]])
#         self$right$generateTree(tree.mat, row[["right daughter"]])
#       } else {
#         # self$elem <- row[["prediction"]]
#         self$elem <- Elem$new(param = self$param)
#       }
#     },
#     predict = function(x) {
#       current.leaf <- self$findLeaf(x, self)
#       # current.leaf$j$elem$stats$pred()
#       current.leaf$j$elem$pred()
#     },
#   ),
#   private = list(
#     poisson__ =  function(lam = 1){
#       l = exp(-lam)
#       loop <- function(k,p) ifelse(p>l, loop(k+1, p*runif(1)), k-1)
#       loop(0,1)
#     },
#     updateOOBE__ = function(x, y) {} # TODO
#   )
# )
# 
# ort1 <- ORT$new(param1, node.ind = 1)
# ort1$generateTree(treemat1) # 37ms, 800KB
# 
# }
# 
# 
# 
# 
# 
# # ORT 第二种写法 -------------
# 
# 
# 
# ORT <- R6Class(
#   classname = "Online Random Tree",
#   inherit = Tree,
#   public = list(
#     param = NULL, age = 0, minSamples = NULL, minGain = NULL,
#     x.rng = NULL, gamma = NULL, numTests = NULL, maxDepth = NULL,
#     numClasses = NULL, classValues = NULL,
#     initialize = function(param) {
#       self$param <- param # only leaf node need
#       self$age <- 0
#       self$minSamples <- param[["minSamples"]]
#       self$minGain <- param[["minGain"]]
#       self$elem<- Elem$new(param = param) # inherit
#       param.names <- names(param)
#       # self$gamma <- ifelse("gamma" %in% param.names, param[["gamma"]], 0) # TODO
#       self$numTests <- ifelse("numTests" %in% param.names, param[["numTests"]], 10)
#       self$maxDepth <- ifelse("maxDepth" %in% param.names, param[["maxDepth"]], 10)
#       self$numClasses <- ifelse("numClasses" %in% param.names, param[["numClasses"]], 0)
#     },
#     findLeaf = function(x, tree, depth = 0) {
#       if (tree$isLeaf()) {
#         return(list(j = tree, depth = depth))
#       } else {
#         x.ind <- tree$elem$splitInd
#         x.val <- tree$elem$splitVal
#         if (x[x.ind] < x.val) {
#           return(self$findLeaf(x, tree$left, depth+1))
#         } else {
#           return(self$findLeaf(x, tree$right, depth+1))
#         }
#       }
#     },
#     gains = function(elem) {
#       tests <- elem$tests
#       on.exit(rm(tests))
#       gain <- function(test) {
#         statsL <- test$statsL; statsR <- test$statsR
#         nL <- statsL$node.n; nR <- statsR$node.n
#         n <- nL + nR + 1e-9
#         lossL <- ifelse(nL==0, 0, statsL$impurity())
#         lossR <- ifelse(nR==0, 0, statsR$impurity())
#         g <- elem$stats$impurity() - (nL/n)*lossL - (nR/n)*lossR
#         max(g, 0)
#       }
#       sapply(tests, gain)
#     },
#     update = function(x, y) {
#       k = rpois(1, lambda = 1)
#       if (k == 0) {
#         self$updateOOBE__(x, y)
#       } else {
#         for (u in seq_len(k)) {
#           self$age <- self$age + 1
#           current.leaf <- self$findLeaf(x, self)
#           j <- current.leaf$j # named by paper, atually the leaf itself
#           depth <- current.leaf$depth
#           j$elem$update(x, y)
#           if (j$elem$stats$node.n > self$minSamples & depth < self$maxDepth) {
#             g <- self$gains(j$elem)
#             if (any(g > self$minGain)) {
#               bestTest <- j$elem$tests[[which.max(g)]] # fuck, j$elem not self$elem !!
#               j$elem$updateSplit(bestTest$xvar.index, bestTest$xvar.value)
#               j$updateChildren(l = Tree$new(Elem$new(self$param)),
#                                r = Tree$new(Elem$new(self$param)))
#               j$left$elem$stats <- bestTest$statsL
#               j$right$elem$stats <- bestTest$statsR
#               j$elem$reset() # for saving memory
#             }
#           }
#         }
#       }
#     },
#     generateTree = function(tree.mat, df.node, node.ind = 1) {
#       super$generateTree(tree.mat, node.ind)
#       private$updateLeafElem(df.node, self, check.y = T)
#     },
#     predict = function(x) {
#       current.leaf <- self$findLeaf(x, self)
#       # current.leaf$j$elem$stats$pred()
#       current.leaf$j$elem$pred()
#     }
#   ),
#   private = list(
#     updateLeafElem = function(df.node, tree, check.y = T) {
#       # check whether y.col is the last col of df.node, only once
#       if (check.y) {
#         y.col.ind <- ncol(df.node)
#         if (self$numClasses > 0) {
#           stopifnot(is.integer(.subset2(df.node, y.col.ind)))
#           stopifnot(min(.subset2(df.node, y.col.ind)) == 1) # TODO
#           self$classValues <- sort(unique(.subset2(df.node, y.col.ind)))
#         } else {
#           stopifnot(is.numeric(.subset2(df.node, y.col.ind)))
#         }
#       }
#       # recursively update leaf node
#       if (tree$isLeaf()) {
#         nrow.df <- nrow(df.node)
#         if (nrow.df == 0) {
#           tree$elem <- Elem$new(param = self$param)
#           return(invisible(NULL))
#         }
#         y.col.ind <- ncol(df.node)
#         tree$elem <- Elem$new(param = self$param)
#         tree$elem$stats$node.n <- nrow.df
# 
#         # update stats and tests in tree$elem
#         if (self$numClasses > 0) {
#           tree$elem$stats$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind), self$classValues)
#           invisible(lapply(tree$elem$tests, function(test) {
#             test.left.inds <- (.subset2(df.node, test$xvar.index) < test$xvar.value)
#             test$statsL$node.n <- sum(test.left.inds)
#             if (test$statsL$node.n > 0) {
#               test$statsL$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind)[test.left.inds], self$classValues)
#             } else {
#               test$statsL$node.counts <- rep(0, self$numClasses)
#             }
#             # test$statsR$node.n <- sum(!test.left.inds)
#             test$statsR$node.n <- nrow.df - test$statsL$node.n
#             if (test$statsR$node.n > 0) {
#               test$statsR$node.counts <- private$valueCounts(.subset2(df.node, y.col.ind)[!test.left.inds], self$classValues)
#             } else {
#               test$statsR$node.counts <- rep(0, self$numClasses)
#             }
#           }))
#         } else {
#           tree$elem$stats$node.sum <- sum(.subset2(df.node, y.col.ind))
#           tree$elem$stats$node.square.sum <- sum(.subset2(df.node, y.col.ind) ^ 2)
#           invisible(lapply(tree$elem$tests, function(test) {
#             test.left.inds <- (.subset2(df.node, test$xvar.index) < test$xvar.value)
#             # maybe need if...else...
#             test$statsL$node.n <- sum(test.left.inds)
#             test$statsL$node.sum <- sum(.subset2(df.node, y.col.ind)[test.left.inds])
#             test$statsL$node.square.sum <- sum(.subset2(df.node, y.col.ind)[test.left.inds] ^ 2)
#             # test$statsR$node.n <- sum(!test.left.inds)
#             test$statsR$node.n <- nrow.df - test$statsL$node.n
#             test$statsR$node.sum <- sum(.subset2(df.node, y.col.ind)[!test.left.inds])
#             test$statsR$node.square.sum <- sum(.subset2(df.node, y.col.ind)[!test.left.inds] ^ 2)
#           }))
#         }
# 
#       } else {
#         left.inds <- (.subset2(df.node, tree$elem$splitInd) < tree$elem$splitVal)
#         private$updateLeafElem(df.node[left.inds, ], tree$left, check.y = F)
#         private$updateLeafElem(df.node[!left.inds, ], tree$right, check.y = F)
#       }
#     },
#     valueCounts = function(y, cls.values) {
#       # cls.values: all the possible class values of y.
#       table(factor(y, levels = cls.values))
#     },
#     poisson__ =  function(lam = 1){
#       l = exp(-lam)
#       loop <- function(k,p) ifelse(p>l, loop(k+1, p*runif(1)), k-1)
#       loop(0,1)
#     },
#     updateOOBE__ = function(x, y) {} # TODO
#   )
# )
# 
# ort2 <- ORT$new(param1)
# ort2$generateTree(treemat1, df.node = dat1) # 23ms, 838KB
# ort2$draw()
# ort2$left$elem$tests[[1]]$statsL
# sapply(1:150, function(i) ort2$predict(dat1[i,1:4]))
# 
# ind.gen <- sample(1:150,50)
# ind.updt <- setdiff(1:150, ind.gen)
# rf3 <- randomForest(factor(Species) ~ ., data=dat1[ind.gen,])
# treemat3 <- getTree(rf3, 33, labelVar=F)
# treemat3 <- cbind(treemat3, node.ind = 1:nrow(treemat3))
# ort3 <- ORT$new(param1)
# ort3$draw()
# ort3$generateTree(treemat3, df.node = dat1[ind.gen,])
# ort3$draw()
# for(i in ind.updt) {
#   ort3$update(dat1[i,1:4], dat1[i,5])
# }
# ort3$draw()
# 
# 
# # regression test
# library(ggplot2)
# dat2 <- as.data.frame(diamonds[sample(1:53000,1000), c(1:6,8:10,7)])
# for (col in c("cut","color","clarity")) dat2[[col]] <- as.integer(dat2[[col]])
# x.rng2 <- data.frame(min = apply(dat2[1:9], 2, min),
#                      max = apply(dat2[1:9], 2, max),
#                      row.names = paste0("X", 1:9))
# param2 <- list('minSamples'= 10, 'minGain'= 1, 'maxDepth' = 10, 'x.rng'= x.rng2)
# ind.gen2 <- sample(1:1000,500)
# ind.updt2 <- setdiff(1:1000, ind.gen2)
# rf2 <- randomForest(price ~ ., data=dat2[ind.gen2,], maxnodes=20)
# treemat2 <- getTree(rf2, 222, labelVar=F)
# treemat2 <- cbind(treemat2, node.ind = 1:nrow(treemat2))
# 
# ort2 <- ORT$new(param2)
# ort2$generateTree(treemat2, df.node = dat2[ind.gen2,])
# ort2$size()
# for (i in ind.updt2) {
#   ort2$update(dat2[i,1:9], dat2[i,10])
# }
# ort2$size()
# 
# 
# 
# # ORF ------------------------
# 
# ORF <- R6Class(
#   classname = "Online Random Forest",
#   public = list(
#     param = NULL, numClasses = NULL, classify = NULL,
#     numTrees = NULL, forest = NULL, # ncores = NULL,
#     initialize = function(param, numTrees = 100) {
#       self$param <- param
#       self$numClasses <- ifelse("numClasses" %in% names(param), param[["numClasses"]], 0)
#       self$classify <- self$numClasses > 0
#       self$numTrees <- numTrees
#       self$forest <- replicate(self$numTrees, ORT$new(param), simplify = F)
#     },
#     update = function(x, y) {
#       # stopifnot(is.numeric(y))
#       ncores = 0
#       if (ncores <= 1) {
#         invisible(lapply(self$forest, function(ort) ort$update(x, y)))
#       } else {
#         # TODO
#         library(parallel)
#         ncores <- min(ncores, detectCores())
#         cluster <- makePSOCKcluster(ncores)
#         on.exit(stopCluster(cluster))
#         clusterExport(cluster, c("Tree","Elem","Test","SuffStats","ORT","ORF"))
#         clusterExport(cluster, c("x","y"), envir = environment())
#         self$forest <- parLapply(cluster, self$forest, function(ort){
#           ort$update(x, y)
#           ort
#         })
#       }
#     },
#     generateForest = function(rf, df.train, y.col) {
#       # df.train: the training data.frame which was used to contruct rf
#       if (!requireNamespace("randomForest", quietly = T))
#         stop("You need install.packages('randomForest') first.")
#       if (!identical(y.col, names(df.train)[ncol(df.train)]))
#         stop("y.col must be the last column of df.train !")
#       if (!("randomForest" %in% class(rf)))
#         stop("rf must be an object constructed by `randomForest::randomForest()` !")
#       if (rf$forest$ntree != self$numTrees)
#         return("The `ntree` parameter in `randomForest::randomForest()` must equal to self$numTrees !")
#       ncore = 0
#       if (ncore <= 1) {
#         invisible(lapply(seq_len(self$numTrees), function(i) {
#           tree.mat <- randomForest::getTree(rf, i, labelVar = F)
#           tree.mat <- cbind(tree.mat, node.ind = 1:nrow(tree.mat))
#           self$forest[[i]]$generateTree(tree.mat, df.node = df.train)
#         }))
#       } else {
#         # TODO
#         library(parallel)
#         ncores <- 4 #min(ncores, detectCores())
#         cluster <- makePSOCKcluster(ncores)
#         on.exit(stopCluster(cluster))
#         clusterExport(cluster, c("Tree","Elem","Test","SuffStats","ORT","ORF"))
#         clusterExport(cluster, c("rf","df.train"), envir = environment())
#         self$forest <- parLapply(cluster, seq_len(self$numTrees), function(i){
#           tree.mat <- randomForest::getTree(rf, i, labelVar = F)
#           tree.mat <- cbind(tree.mat, node.ind = 1:nrow(tree.mat))
#           ort <- self$forest[[i]]
#           ort$generateTree(tree.mat, df.node = df.train)
#           ort
#         })
#       }
#     },
#     predict = function(x) {
#       preds <- unlist(sapply(self$forest, function(ort) ort$predict(x), USE.NAMES = F))
#       if (self$classify) {
#         pred.counts <- table(preds)
#         return(as.integer(names(pred.counts)[which.max(pred.counts)]))
#       } else {
#         return(mean(preds))
#       }
#     },
#     predicts = function(X) {
#       stopifnot(is.matrix(X) | is.data.frame(X))
#       apply(X, 1, self$predict)
#     },
#     predStat = function() { "TODO Later" },
#     meanTreeSize = function() {
#       mean(sapply(self$forests, function(ort) ort$tree$size()))
#     },
#     meanNumLeaves = function() {
#       mean(sapply(self$forests, function(ort) ort$tree$numLeaves()))
#     },
#     meanTreeDepth = function() {
#       mean(sapply(self$forests, function(ort) ort$tree$depth()))
#     },
#     sdTreeSize = function() {
#       sd(sapply(self$forests, function(ort) ort$tree$size()))
#     },
#     sdNumLEaves = function() {
#       sd(sapply(self$forests, function(ort) ort$tree$numLeaves()))
#     },
#     sdTreeDepth = function() {
#       sd(sapply(self$forests, function(ort) ort$tree$depth()))
#     },
#     confusionMatrix = function(X, y, pretty = FALSE) {
#       stopifnot(is.matrix(X) | is.data.frame(X))
#       if (!self$classify)
#         return("Confusion matrices can only be obtained for classification problem.")
#       preds <- self$predicts(X)
#       if (!pretty) {
#         return(table(pred = preds, y = y))
#       } else {
#         if (!requireNamespace("gmodels", quietly = T)) {
#           return("You need install.packages('gmodels') first.")
#         } else {
#           return(
#             gmodels::CrossTable(preds, y, prop.chisq = FALSE, prop.t = FALSE,
#                                 dnn = c('prediction', 'actual'))
#           )
#         }
#       }
#     }
#   )
# )
# 
# rf2 <- randomForest(price ~ ., data=dat2[ind.gen2,], maxnodes=20, ntree = 100)
# orf2 <- ORF$new(param2)
# orf2$generateForest(rf2, df.train = dat2[ind.gen2,], y.col = "price") # 9s,114MB
# for (i in sample(ind.updt2,100)) {
#   orf2$update(dat2[i,1:9], dat2[i,10])
# } # 35s
# 
# preds2 <- orf2$predicts(dat2[901:1000, 1:9])
# Metrics::rmse(preds2, dat2$price[901:1000]) # 995.9
# 
# preds.rf2 <- predict(rf2, newdata = dat2[901:1000,])
# Metrics::rmse(preds.rf2, dat2$price[901:1000]) # 1116.4
# 
# 
# # ------------
# 
# # TODO list
# # 1. node.counts目前只支持分类问题的类别从1,2,...开始
# # 2. OOBE更新树、rpart to ORT
# # 3. 并行计算
# 
# 
# 
# 
# 
# # --------
# 
# 
# 
# 
# 









