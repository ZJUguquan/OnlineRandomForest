

library(R6)
library(pryr)

# 面向对象编程的好处就是，当复制一个对象A的元素a，命名为a2，并对a2进行更改，
#   那么原先对象（A）中的对应元素（A.a）也会发生更改。
#   而删除a2的时候，A.a则不会发生变化！

# Tree ---------------

Tree <- R6Class(
  classname = "Tree",
  public = list(
    elem = NULL, left = NULL, right = NULL,
    initialize = function(elem, left = NULL, right = NULL) {
      stopifnot((is.null(left) & is.null(right)) | 
                sum(c(class(left), class(right)) == "Tree") == 2)
      self$elem <- elem
      self$left <- left
      self$right <- right
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
      if (self$isLeaf()) 1 else { max(self$left$depth(), self$right$depth()) + 1 }
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


ta <- Tree$new("abc",NULL,NULL)
tb <- Tree$new(1,Tree$new(36),Tree$new(3))
tc <- Tree$new(89, tb, ta)
td <- Tree$new("guquan", tc, tb)

tb$draw()
tb$size()
tb$maxDepth()
tc$draw()
tc$maxDepth()
tc$right$updateChildren(Tree$new("666"), Tree$new(999))
tc$right$right$updateChildren(Tree$new("666"), Tree$new(999))
tc$draw()
td$draw()






# SuffStats 统计量 -----
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
        self$node.counts[y] <- self$node.counts[y] + 1
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
        # 如果频次一样怎么办？
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

sa <- SuffStats$new(numClasses = 3)
sa$update(2);sa$update(1);sa$update(2)
sa$pred()
sa$node.counts
sa$node.n
sa$impurity()

sb <- SuffStats$new(numClasses = 0)
sb$update(1);sb$update(2);sb$update(3)
sb$pred()
sb$node.sum;sb$node.square.sum
sb$impurity()


# Test 候选分裂 -----
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

t1 <- Test$new(3, 2.2, 0)
t1$update(c(0,0,2,4), 1.2)
t1$update(c(0,0,1,4), 1.4)
t1$update(c(1,1,3,3), 2.7)
t1$statsL$pred()
t1$statsR$pred()


# Elem 节点存储的内容 -----
Elem <- R6Class(
  classname = "Elem",
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
    # split = function() {
    #   list(splitInd = self$splitInd, splitVal = self$splitVal)
    # }
  )
)
# param2 <- param
# param2[["numTests"]] <- 1e4
# elm2 <- Elem$new(param2)

x.rng <- data.frame(min = c(1,3,5,7), max = c(2,4,6,8), row.names = paste0("x",1:4))
param <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng)
elm1 <- Elem$new(param)
length(elm1$tests)
elm1$tests[c(1,8)]
elm1$updateSplit(3, 5.2)
elm1$toString()
elm1$update(c(1.1, 3.2, 5.3, 7.4), 1)
elm1$update(c(1.11, 3.22, 5.33, 7.44), 1)
elm1$update(c(1.9, 3.9, 5.9, 7.9), 3)
system.time(replicate(10000,elm1$update(c(1.1, 3.2, 5.3, 7.4), 1)))



# Online RT ----------
ORT <- R6Class(classname = "Online Random Tree",
  public = list(
    param = NULL, age = 0, minSamples = NULL, minGain = NULL, 
    x.rng = NULL, gamma = NULL, numTests = NULL, numClasses = NULL,
    maxDepth = NULL, tree = NULL,
    initialize = function(param) {
      self$param <- param
      self$age <- 0
      self$minSamples <- param[["minSamples"]]
      self$minGain <- param[["minGain"]]
      self$x.rng <- param[["x.rng"]] # not use
      # why not inherit ?
      self$tree<- Tree$new(Elem$new(param = param))
      # parameters gamma TODO
      param.names <- names(param)
      self$gamma <- ifelse("gamma" %in% param.names, param[["gamma"]], 0)
      self$numTests <- ifelse("numTests" %in% param.names, param[["numTests"]], 10)
      self$numClasses <- ifelse("numClasses" %in% param.names, param[["numClasses"]], 0)
      self$maxDepth <- ifelse("maxDepth" %in% param.names, param[["maxDepth"]], 10)
    },
    draw = function() {
      self$tree$draw()
      # if(identical(parent.frame(), .GlobalEnv))
        # on.exit(cat("\nNote: if condition satisfied, go L(Left), otherwise R(Right).\n"))
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
        self$updateOOBE__(x, y)
      } else {
        for (u in seq_len(k)) {
          self$age <- self$age + 1
          current.leaf <- self$findLeaf(x, self$tree)
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
    predict = function(x) {
      current.leaf <- self$findLeaf(x, self$tree)
      # current.leaf$j$elem$stats$pred()
      current.leaf$j$elem$pred()
    },
    poisson__ =  function(lam = 1){
      l = exp(-lam)
      loop <- function(k,p) ifelse(p>l, loop(k+1, p*runif(1)), k-1)
      loop(0,1)
    }, # just use rpois(1,1), faster
    updateOOBE__ = function(x, y) {} # TODO
  )
)

dat1 <- iris
dat1$class <- as.integer(dat1$Species)
x.rng1 <- data.frame(min = apply(dat1[1:4], 2, min), 
                     max = apply(dat1[1:4], 2, max), 
                     row.names = paste0("X",1:4))
param1 <- list('minSamples'= 5, 'minGain'= 0.1, 'numClasses'= 3, 'x.rng'= x.rng1)
ort1 <- ORT$new(param1)
# ORT's building is strongly influenced by the sequence order of samples.
# 0.17 s, 601 kB
for (i in sample(c(1:40,51:90,101:140), 120)) {
  ort1$update(dat1[i,1:4], dat1[i,6])
} 
ort1$draw()
ort1$predict(dat1[44,1:4])
ort1$predict(dat1[91,1:4])
ort1$predict(dat1[148,1:4])

table(sapply(c(41:50,91:100,141:150), function(i) ort1$predict(dat1[i,1:4])),
      dat1[c(41:50,91:100,141:150), 6])

# ort3$update(dat3[1,1:5], dat3[1,6])
# ort3$update(dat3[52,1:5], dat3[52,6])
# g <- ort3$gains(ort3$tree$elem)

# ort1$update(dat[53,1:4], dat[53,6])
# ort1$update(dat[104,1:4], dat[104,6])
# ort1$tree$elem$numSamplesSeen
# g <- ort1$gains(ort1$tree$elem)
# j <- ort1$tree
# bestTest <- j$elem$tests[[which.max(g)]]
# j$elem$splitInd
# ort1$tree$elem$splitInd
# j$elem$updateSplit(bestTest$xvar.index, bestTest$xvar.value)


library(ggplot2)
dat2 <- diamonds[sample(1:53000,1000), c(1:6,8:10,7)]
dat22 <- as.data.frame(dat2) # 3 times faster
for (col in c("cut","color","clarity")) dat2[[col]] <- as.integer(dat2[[col]])
x.rng2 <- data.frame(min = apply(dat2[1:9], 2, min), 
                     max = apply(dat2[1:9], 2, max), 
                     row.names = paste0("X", 1:9))
param2 <- list('minSamples'= 20, 'minGain'= 3, 'maxDepth' = 5, 'x.rng'= x.rng2)
ort2 <- ORT$new(param2)
for (i in sample(1:900,900)) {
  ort2$update(dat2[i,1:9], dat2[i,10])
} # 2.75s, long time
ort2$draw()
pred2 <- sapply(901:1000, function(i) {
  ort2$predict(dat2[i,1:9])
})
rmse(pred2, dat2$price[901:1000])
plot(pred2, dat2$price[901:1000])

# compare
library(rpart)
rpt2 <- rpart(price ~ ., dat2[1:900,])
pred2.rpt <- predict(rpt2, dat2[901:1000,])
rmse(pred2.rpt, dat2$price[901:1000]) # 1396
plot(pred2.rpt, dat2$price[901:1000])



# not a good example --------
dat3 <- airquality
dat3 <- dat3[complete.cases(dat3), c(1,2,3,5,6,4)]
x.rng3 <- data.frame(min = apply(dat3[1:5], 2, min), 
                     max = apply(dat3[1:5], 2, max), 
                     row.names = paste0("X",1:5))
param3 <- list('minSamples'= 5, 'minGain'= 3, 'maxDepth' = 4, 'x.rng'= x.rng3)
ort3 <- ORT$new(param3)
for (i in sample(1:100,100)) {
  ort3$update(dat3[i,1:5], dat3[i,6])
} 
ort3$draw()
ort3$predict(dat3[101,1:5]);dat3[101,6]
pred11 <- sapply(101:111, function(i) {
  ort3$predict(dat3[i,1:5])
})
library(Metrics)
rmse(pred11, dat3$Temp[101:111])




# Online RF -------------------
ORF <- R6Class(
  classname = "Online Random Forest",
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
    update = function(x, y, ncores = 0) {
      # stopifnot(is.numeric(y))
      if (ncores > 1) {
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
      } else {
        invisible(lapply(self$forest, function(ort) ort$update(x, y)))
      }
    },
    predict = function(x) {
      preds <- unlist(sapply(self$forest, function(ort) ort$predict(x), USE.NAMES = F))
      if (self$classify) {
        pred.counts <- table(preds)
        return(as.integer(names(pred.counts)[which.max(pred.counts)]))
      } else {
        return(mean(preds))
      }
    },
    predicts = function(X) {
      stopifnot(is.matrix(X) | is.data.frame(X))
      apply(X, 1, self$predict)
    },
    predStat = function() { "TODO Later" },
    meanTreeSize = function() {
      mean(sapply(self$forests, function(ort) ort$tree$size()))
    },
    meanNumLeaves = function() {
      mean(sapply(self$forests, function(ort) ort$tree$numLeaves()))
    },
    meanTreeDepth = function() {
      mean(sapply(self$forests, function(ort) ort$tree$depth()))
    },
    sdTreeSize = function() {
      sd(sapply(self$forests, function(ort) ort$tree$size()))
    },
    sdNumLEaves = function() {
      sd(sapply(self$forests, function(ort) ort$tree$numLeaves()))
    },
    sdTreeDepth = function() {
      sd(sapply(self$forests, function(ort) ort$tree$depth()))
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

# 测试

orf1 <- ORF$new(param1, numTrees = 20)
# 16.52 s, 71.6 MB
for (i in sample(c(1:40,51:90,101:140), 120)) {
  orf1$update(dat1[i,1:4], dat1[i,6])
} 
orf1$forest[[4]]$tree$draw()
orf1$confusionMatrix(dat1[c(41:50,91:100,141:150), 1:4],
                     dat1[c(41:50,91:100,141:150), 6], pretty = T)


# 1min, 25.3M
orf2 <- ORF$new(param2, numTrees = 20)
for (i in sample(1:900,900)) {
  orf2$update(dat2[i,1:9], dat2[i,10])
}
orf2$forest[[4]]$tree$draw()
preds2 <- orf2$predicts(dat2[901:1000,1:9])
rmse(preds2, dat2$price[901:1000]) # 1303
plot(preds2, dat2$price[901:1000]) 


# compare 
library(randomForest)
rf2 <- randomForest(price ~ ., data = dat2[1:900,], ntree = 20) # 537KB
preds.rf2 <- predict(rf2, newdata = dat2[901:1000,])
rmse(preds.rf2, dat2$price[901:1000]) # 905
plot(preds.rf2, dat2$price[901:1000])


# parallel test
orf3 <- ORF$new(param1, numTrees = 100)
inds <- sample(1:150, 20)
for(i in inds) orf3$update(dat1[i,1:4], dat1[i,6], ncores = 0) # 2.52s, 22.6MB
orf3$forest[[3]]$tree$draw()
for(i in inds) orf3$update(dat1[i,1:4], dat1[i,6], ncores = 4) # 10min, 961MB, SB parallel !




# ORT inherit Tree -----------

ORT.in <- R6Class(
  inherit = Tree,
  public = list(
    param = NULL, age = 0, minSamples = NULL, minGain = NULL, 
    x.rng = NULL, gamma = NULL, numTests = NULL, numClasses = NULL,
    maxDepth = NULL,
    initialize = function(param) {
      self$param <- param
      self$age <- 0
      self$minSamples <- param[["minSamples"]]
      self$minGain <- param[["minGain"]]
      # self$x.rng <- param[["x.rng"]] # not use
      self$elem<- Elem$new(param = param) # inherit
      # parameters gamma TODO
      param.names <- names(param)
      self$gamma <- ifelse("gamma" %in% param.names, param[["gamma"]], 0)
      self$numTests <- ifelse("numTests" %in% param.names, param[["numTests"]], 10)
      self$numClasses <- ifelse("numClasses" %in% param.names, param[["numClasses"]], 0)
      self$maxDepth <- ifelse("maxDepth" %in% param.names, param[["maxDepth"]], 10)
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
        self$updateOOBE__(x, y)
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
    predict = function(x) {
      current.leaf <- self$findLeaf(x, self)
      # current.leaf$j$elem$stats$pred()
      current.leaf$j$elem$pred()
    },
    updateOOBE__ = function(x, y) {} # TODO
  ),
  private = list(
    poisson__ =  function(lam = 1){
      l = exp(-lam)
      loop <- function(k,p) ifelse(p>l, loop(k+1, p*runif(1)), k-1)
      loop(0,1)
    }
  )
)


ort.in2 <- ORT.in$new(param2)
for (i in sample(1:900,900)) {
  ort.in2$update(dat2[i,1:9], dat2[i,10])
} # 3.11s, a little longer
# ort.in2 <- ORT.in$new(param2)
# invisible(lapply(sample(1:900,900), function(i) ort.in2$update(dat2[i,1:9], dat2[i,10])))


ort.in2$draw()
pred.in2 <- sapply(901:1000, function(i) {
  ort.in2$predict(dat2[i,1:9])
})
rmse(pred.in2, dat2$price[901:1000])
plot(pred.in2, dat2$price[901:1000])






# 并行计算：修改当前变量测试 ------------------
library(parallel)
cl <- makePSOCKcluster(2)

tt <- R6Class(
  public = list(
    x = 6,
    update = function(n) self$x <- self$x + n
  )
)
tts <- R6Class(
  public = list(
    ts = replicate(4, tt$new(), simplify = F),
    update = function(n=10, type=1) {
      if(type==1) {
        invisible(lapply(self$ts, function(t) t$update(n)))
      }
      else {
        clusterExport(cl, c("n"), envir = environment())
        self$ts <- parLapply(cl, self$ts, function(t) {t$update(n);t})
      }
    }
  )
)
ts1 <- tts$new()
ts1$update()
ts1$ts
m=5
ts1$update(m, 2)



































