# Tree ------------------------

#' @title Create a Tree Object 
#' @description Tree is a class of R6. You can use it to create a binary tree via diffrent ways.
#' @author Quan Gu
#' 
#' @usage Tree$new(elem = NULL, left = NULL, right = NULL, node.ind = 1)
#' @param elem The element in the tree. More details show in \code{help(Elem)}. Default NULL.
#' @param left The left child of the Tree. Also a Tree oject. Default NULL.
#' @param right The right child of the Tree. Also a Tree oject. Default NULL.
#' @param node.ind The index of the current node in Tree. Default \code{1} for the root node.
#' 
#' @details See details in description of each field or method.
#' @return Object of \code{\link{R6Class}}, Object of \code{Tree}.
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
  cloneable = FALSE, portable = FALSE, class = FALSE,
  public = list(
    elem = NULL, left = NULL, right = NULL, node.ind = NULL,
    initialize = function(elem = NULL, left = NULL, right = NULL, node.ind = 1) {
      stopifnot((is.null(left) & is.null(right)) | 
                 sum(c(class(left), class(right)) %in% c("Tree", "environment")) == 2)
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



