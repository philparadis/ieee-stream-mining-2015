######################################
switch(Sys.info()[['sysname']],
       # On Windows, this is Yue's directory
       Windows = {  work.dir <- "C:/Users/Yue/Desktop/Research_Project/research/stream-mining" },
       # On Linux, this is Phil's directory
       Linux = { work.dir <- "~/h/proj/ieee-stream-mining/code"  })
setwd(work.dir)
source("dodgers-load.r")

##############################
# Ideas and notes
##############################

# TODO: We could try changing the classification task. Instead of predicting
# if a game happened or not on a given day, we could try to predict whether
# (a) no game happened that day, (b) a game happened in the morning, (c) a
# game happened in the afternoon, or (d) a game happened in the evening
#
# Or, we could try to predict between 25 classes. Class 0-23 indicate the
# hour at which a game happened. Class 24 indicates no game on that day.


# TODO: Experiment with 'streaming PCA' from the package (???) for
# dimensionality reduction.


#############################
# Start training classifiers
#############################
library(nnet)
library(caret)
library(randomForest)
library(rpart)
library(tree)
library(e1071)
library(class)

# Constant variables
levels.binary <- as.factor(c(0, 1))

# Setup global variables
train <- NULL
test <- NULL
validate <- NULL
train.data <- NULL
test.data <- NULL
validate.data <- NULL
train.labels <- NULL
test.labels <- NULL
validate.labels <- NULL

global.setup.dataset <- NULL
global.setup.train.num <- -1
global.setup.test.num <- -1
global.setup.validate.num <- -1
global.setup.discard.bad.rows <- -1
global.setup.use.weekend.feature <- -1
global.setup.weekend.weight <- -1
global.setup.shuffle.rows <- -1
global.setup.shuffle.seed <- -1
global.setup.K <- -1 # Maximum number of models per ensemble

############################################################################
# This function initialize the training, testing and validation sets.
# IMPORTANT: There are 175 training examples, so setup.train.num +
# setup.test.num must be less than or equal to 175 (strictly less if a
# validation set is desired).
# HOWEVER, if using 'discard.bad.rows = TRUE', then rows with more than 10
# missing values will be discarded. This will leave a total of 151 training
# examples. Make sure to choose the training/testing sizes accordingly.
############################################################################
setup.test.train <- function(dataset,
                             train.num = 90,
                             test.num = 40,
                             discard.bad.rows = TRUE,
                             use.weekend.feature = TRUE,
                             weekend.weight = 1.0,
                             shuffle.rows = FALSE,
                             shuffle.seed = NULL,
                             K = 70)
{
   global.setup.dataset <<- dataset
   global.setup.train.num <<- train.num
   global.setup.test.num <<- test.num
   global.setup.discard.bad.rows <<- discard.bad.rows
   global.setup.use.weekend.feature <<- use.weekend.feature
   global.setup.weekend.weight <<- weekend.weight
   global.setup.shuffle.rows <<- shuffle.rows
   global.setup.shuffle.rows <<- shuffle.seed
   global.setup.K <<- K

   if (discard.bad.rows) {
      new.data <- dataset[-days.discard,]
      row.names(new.data) <- NULL
      new.num.days <- nrow(new.data)
   } else {
      new.data <- dataset
      new.num.days <- global.num.days
   }
   
   global.setup.validate.num <<- new.num.days - train.num - test.num
   
   if (shuffle.rows) {
      set.seed(shuffle.seed)
      new.rows <- sample(nrow(new.data))
      new.data <- new.data[new.rows, ]
   }
   
   train <<- new.data[1:train.num, ]
   test <<- new.data[(train.num+1):(train.num+test.num), ]
   validate <<- new.data[(train.num+test.num+1):(train.num+test.num+global.setup.validate.num), ]
   
   if (use.weekend.feature) {
      train.data <<- data.frame(is.weekend=train$is.weekend*weekend.weight, do.call(rbind, train$data))
      test.data <<- data.frame(is.weekend=test$is.weekend*weekend.weight, do.call(rbind, test$data))
      validate.data <<- data.frame(is.weekend=validate$is.weekend*weekend.weight, do.call(rbind, validate$data))
   } else {
      train.data <<- data.frame(do.call(rbind, train$data))
      test.data <<- data.frame(do.call(rbind, test$data))
      validate.data <<- data.frame(do.call(rbind, validate$data))
   }
   
   train.labels <<- factor(train$class, levels=levels.binary)
   test.labels <<- factor(test$class, levels=levels.binary)
   validate.labels <<- factor(validate$class, levels=levels.binary)
}

get.current.setup <- function ()
{
   list(dataset = global.setup.dataset,
        train.num = global.setup.train.num,
        test.num = global.setup.test.num,
        discard.bad.rows = global.setup.discard.bad.rows,
        use.weekend.feature = global.setup.use.weekend.feature,
        weekend.weight = global.setup.weekend.weight,
        shuffle.rows = global.setup.shuffle.rows,
        shuffle.seed = global.setup.shuffle.seed,
        K = global.setup.K,
        system.time = Sys.time())
}

#################################
# BENCHMARKS FUNCTION DEFINITION
#################################

# Random forests on full training data
run.benchmarks <- function(train.data, train.labels, test.data, test.labels)
{
   results <- data.frame()
   
   # Coin flipping
   model <- NULL
   pred <- factor(sample(0:1, length(train.labels)+length(test.labels), replace=TRUE),
                  levels=levels.binary)
   cmo <- confusionMatrix(pred[1:length(train.labels)], train.labels)
   cm <- confusionMatrix(pred[(length(train.labels)+1):(length(train.labels)+length(test.labels))], test.labels)
   cat(paste("FULL TRAINING DATA: 'coin flip' Accuracy =          ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="coin flip", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Always pick class 0
   model <- NULL
   pred <- factor(rep(0, max(length(train.labels), length(test.labels))), levels=levels.binary)
   cmo <- confusionMatrix(pred[1:length(train.labels)], train.labels)
   cm <- confusionMatrix(pred[1:length(test.labels)], test.labels)
   cat(paste("FULL TRAINING DATA: 'always 0' Accuracy =           ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="always 0", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Always pick class 1
   model <- NULL
   pred <- factor(rep(1, max(length(train.labels), length(test.labels))), levels=levels.binary)
   cmo <- confusionMatrix(pred[1:length(train.labels)], train.labels)
   cm <- confusionMatrix(pred[1:length(test.labels)], test.labels)
   cat(paste("FULL TRAINING DATA: 'always 1' Accuracy =           ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="always 1", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Random forest on full training data
   model <- randomForest(train.data, train.labels,
                         ntree=100)
   cmo <- confusionMatrix(unname(predict(model, train.data)), train.labels)
   cm <- confusionMatrix(unname(predict(model, test.data)), test.labels)
   cat(paste("FULL TRAINING DATA: 'randomForest' Accuracy =       ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="randomForest", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Decision tree on full training data
   model <- rpart(train.labels ~ ., data=train.data, method="class")
   cmo <- confusionMatrix(1-unname(round(predict(model, train.data)[,1])), train.labels)
   cm <- confusionMatrix(1-unname(round(predict(model, test.data)[,1])), test.labels)
   cat(paste("FULL TRAINING DATA: 'rpart' Decision Tree Accuracy =", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="rpart", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Decision tree on full training data
   model <- tree(train.labels ~ ., data=train.data)
   cmo <- confusionMatrix(1-unname(round(predict(model, train.data)[,1])), train.labels)
   cm <- confusionMatrix(1-unname(round(predict(model, test.data)[,1])), test.labels)
   cat(paste("FULL TRAINING DATA: 'tree' Decision Tree Accuracy = ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="tree", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # svm on full training data
   model <- svm(train.labels ~ ., data=train.data)
   cmo <- confusionMatrix(unname(predict(model, train.data)), train.labels)
   cm <- confusionMatrix(unname(predict(model, test.data)), test.labels)
   cat(paste("FULL TRAINING DATA: 'svm' Accuracy =                ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="svm", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # knn on full training data
   model <- knn(train.data, test.data, train.labels,
                k=3)
   cm <- confusionMatrix(model, test.labels)
   model <- knn(train.data, train.data, train.labels,
                k=3)
   cmo <- confusionMatrix(model, train.labels)
   cat(paste("FULL TRAINING DATA: 'knn' Accuracy =                ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="knn", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # Naive Bayes on full training data
   model <- naiveBayes(train.labels ~ ., data=train.data)
   cmo <- confusionMatrix(unname(predict(model, train.data)), train.labels)
   cm <- confusionMatrix(unname(predict(model, test.data)), test.labels)
   cat(paste("FULL TRAINING DATA: 'naiveBayes' Accuracy =         ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="naiveBayes", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))
   
   # LDA on full training data
#    model <- lda(train.labels ~ ., data=train.data)
#    cm <- confusionMatrix(unname(predict(model, test.data)$class), test.labels)
#    cat(paste("FULL TRAINING DATA: 'lda' Accuracy =                ", cm$overall[[1]], "\n"))
#    results <- rbind(results, data.frame(name="lda", acc=cm$overall[[1]]))
   
   # Neural Nets
   invisible(capture.output(model <- nnet(train.labels ~ ., data=train.data,
                 MaxNWts=5000, size=4, maxit=300, decay=0.01)))
   cmo <- confusionMatrix(unname(round(predict(model, train.data))), train.labels)
   cm <- confusionMatrix(unname(round(predict(model, test.data))), test.labels)
   cat(paste("FULL TRAINING DATA: 'nnet' Accuracy =               ", cm$overall[[1]], "\n"))
   results <- rbind(results, data.frame(name="nnet", train.acc=cmo$overall[[1]], test.acc=cm$overall[[1]]))

   results
}

########################################
# Here we run some standard benchmarks.
########################################

# # Run some tests
# setup_test_train(data=d.byday, use.weekend.feature = FALSE)
# ra1 <- run_benchmarks(train.data, train.labels, validate.data, validate.labels)
# setup_test_train(data=d.byday.30min, use.weekend.feature = FALSE)
# ra2 <- run_benchmarks(train.data, train.labels, validate.data, validate.labels)
# setup_test_train(data=d.byday.1hour, use.weekend.feature = FALSE)
# ra3 <- run_benchmarks(train.data, train.labels, validate.data, validate.labels)
# cbind(ra1, ra2, ra3)
# 
# # Run some tests to find the "importance" of is.weekend
# setup_test_train(data=d.byday)
# rb1 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# setup_test_train(data=d.byday.30min)
# rb2 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# setup_test_train(data=d.byday.1hour)
# rb3 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# cbind(rb1, rb2, rb3)
# 
# # Run some tests to find how rows with lots of missing values
# # affect results
# setup_test_train(data=d.byday, discard.bad.rows = TRUE)
# rc1 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# setup_test_train(data=d.byday.30min, discard.bad.rows = TRUE)
# rc2 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# setup_testwin.size_train(data=d.byday.1hour, discard.bad.rows = TRUE)
# rc3 <- run_benchmarks(train.data, train.labels, test.data, test.labels)
# cbind(rc1, rc2, rc3)

#########################################################################
# Generate a collection of indices that describe the sliding windows
#########################################################################
get.windows.indices.list <- function (windows.length)
{
   ind.windows.length <- windows.length
   ind.windows.num <- global.setup.train.num - ind.windows.length + 1
   ind.windows <- lapply(0:(ind.windows.num-1), function(x) 1:ind.windows.length+x)
   ind.windows
}

#########################################################################
# Run random forest on sliding windows. This is not really relevant to
# our project. The function was written for testing purposes.
#########################################################################
run.sliding.windows.rf <- function(windows.length, validate=FALSE)
{
   ind.windows <- get.windows.indices.list(windows.length)
   
   if (validate) {
      my.test.data <- validate.data
      my.test.labels <- validate.labels
      my.mode <- "VALIDATE"
   } else { 
      my.test.data <- test.data
      my.test.labels <- test.labels
      my.mode <- "  TEST  "
   }
   
   train.acc <- list()
   acc <- list()
   for (ind in ind.windows) {
      window.data <- train.data[ind,]
      window.class <- train.labels[ind]
      
      if (count.uniques(as.numeric(window.class)) == 1) {
         warning(paste0("Skipped window with indices '", paste(ind, collapse=","),
                       "' because all classes are the same..."))
         next
      }
      
      model.rf <- randomForest(window.data, window.class,
                               ntree=500, keep.forest=TRUE)
      pred.rf <- predict(model.rf, my.test.data)
      
      cm.rf <- confusionMatrix(unname(pred.rf), my.test.labels)
      #cat(paste("Random Forest Accuracy =", cm.rf$overall[[1]], "\n"))
      train.acc[[length(train.acc)+1]] <- confusionMatrix(unname(predict(model.rf, window.data)), window.class)$overall[[1]]
      acc[[length(acc)+1]] <- cm.rf$overall[[1]]
   }
   cat(paste0("Using sliding windows of size ", windows.length, " days...\n"))
   cat(paste("Mean Random Forest TRAINING data accuracy           =", mean(unlist(train.acc)), "\n"))
   cat(paste("Mean Random Forest",my.mode,"data set accuracy       =", mean(unlist(acc)), "\n"))
}


###########################################################################
# This is the code for our new method using ensemble of decision trees
# built individually on sliding windows. The function 'pred.trees.list'
# is used to carry out plurality voting on the collection of trees for new
# data
###########################################################################
pred.trees.list <- function (trees, test)
{
   votes.class0 <- c()
   for (model in tail(trees, global.setup.K)) {
      pred <- predict(model, test)
      pred.class0 <- pred[,1]
      votes.class0 <- cbind(votes.class0, round(unname(pred.class0)))
   }
   factor(1-round(unname(rowMeans(votes.class0))), levels=levels.binary)
}
##############################################################################
# Note: It is not recommended to go below 10 for 'sliding.window.size' when
# using the Dodgers Loop Dataset, as the sliding windows will frequently
# contain only 1 class example or be very unbalanced
##############################################################################
run.sliding.windows.ens.trees <- function(sliding.window.size, validate=FALSE)
{
   ind.windows <- get.windows.indices.list(sliding.window.size)
   
   if (validate) {
      my.test.data <- validate.data
      my.test.labels <- validate.labels
      my.mode <- "VALIDATE"
   } else { 
      my.test.data <- test.data
      my.test.labels <- test.labels
      my.mode <- "  TEST  "
   }
   
   train.acc <- list()
   acc <- list()
   trees <- list()
   earlier.data <- c()
   earlier.class <- factor(levels=levels.binary)
   earlier.num <- 0
   for (ind in ind.windows) {
      window.data <- rbind(earlier.data, train.data[ind,])
      window.class <- factor(c(as.character(earlier.class),
                               as.character(train.labels[ind])),
                             levels=levels.binary)
      
      # Check if the classes are sufficiently balanced in this sliding window
      unbalanced.classes <- FALSE
      if (count.uniques(as.numeric(window.class)) == 1) {
         unbalanced.classes <- TRUE
      } else {
         class.0.num <- table(window.class)[[1]]
         class.1.num <- table(window.class)[[2]]
         if (table(window.class)[[1]] < 2 || table(window.class)[[2]] < 2) {
            unbalanced.classes <- TRUE
         }
      }
      
      if(unbalanced.classes == TRUE) { 
         # Skip sliding window because it only contains examples from the same class
         # Or because the classes are too unbalanced
         # Save first example to be used in the next sliding window
         earlier.num <- earlier.num + 1
         earlier.data <- rbind(earlier.data, window.data[earlier.num,])
         earlier.class[earlier.num] <- window.class[earlier.num]
         next
      } else {
         earlier.data <- c()
         earlier.class <- factor(levels=levels.binary)
         earlier.num <- 0
      }
      
      # Construct model
      model.tree <- tree(window.class ~ ., data=window.data)
      pred.tree <- factor(1-round(unname(predict(model.tree, my.test.data)[,1])), levels=levels.binary)
      
      cm.tree <- confusionMatrix(pred.tree, my.test.labels)
      #cat(paste("Decision Tree Accuracy =", cm.tree$overall[[1]], "\n"))
      trees[[length(trees)+1]] <- model.tree
      train.acc[[length(train.acc)+1]] <- confusionMatrix(factor(1-round(unname(predict(model.tree, window.data)[,1])),
                                                                 levels=levels.binary), window.class)$overall[[1]]
      acc[[length(acc)+1]] <- cm.tree$overall[[1]]
   }
   cat(paste0("Stream Decision Trees Classification (sliding.window.size = ", sliding.window.size, " days)\n"))
   cat(paste("Mean TRAINING data accuracy                  =", mean(unlist(train.acc)), "\n"))
   cat(paste("Mean", my.mode ,"accuracy                       =", mean(unlist(acc)), "\n"))
   
   # Ensemble of decision trees
   pred.ensemble.trees <- pred.trees.list(trees, my.test.data)
   cm.ensemble.trees <- confusionMatrix(pred.ensemble.trees, my.test.labels)
   cat(paste("Ensemble of Decision Trees", my.mode, "Accuracy =", cm.ensemble.trees$overall[[1]], "\n"))
   
   # Return mean training accuracy and ensembe of trees test accuracy
   c(mean(unlist(train.acc)), cm.ensemble.trees$overall[[1]])
}

# setup_test_train(d.byday.1hour, setup.train.num=90)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.rf(j)
#    run.sliding.windows.rf(j, validate=TRUE)
# }
# 
# 
# setup_test_train(d.byday, setup.train.num=90)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# setup_test_train(d.byday, setup.train.num=90, discard.bad.rows=TRUE)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# setup_test_train(d.byday.30min, setup.train.num=90)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# setup_test_train(d.byday.30min, setup.train.num=90, discard.bad.rows=TRUE)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# 
# setup_test_train(d.byday.1hour, setup.train.num=90)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# setup_test_train(d.byday.1hour, setup.train.num=90, setup.test.num=30, discard.bad.rows=TRUE)
# for (j in c(15,20,25,30,35,40,45,50)) {
#    run.sliding.windows.ens.trees(j)
# }
# 
# # Best settings (probably)
# for (i in 1:10) {
#    setup_test_train(d.byday.30min, setup.train.num=90, setup.test.num=30,
#                     discard.bad.rows=TRUE, shuffle.rows=TRUE)
#    run.sliding.windows.ens.trees(30)
#    run.sliding.windows.ens.trees(30, validate=TRUE)
# }
# 
# # Other experiment: Overfitting
# # It looks like we might be overfitting even with decision trees. See below.
# # Need to investigate more.
# setup_test_train(d.byday, 30, 30, shuffle.rows = TRUE, discard.bad.rows = TRUE)
# run.sliding.windows.ens.trees(30)


#################################################################
# Definition of Sliding Windows Neural Net (no ensemble method)
# The weights are passed from one sliding window to the other.
#
# Note: It is not recommended to go below 10 for 'sliding.window.size' when
# using the Dodgers Loop Dataset, as the sliding windows will frequently
# contain only 1 class example or be very unbalanced
##############################################################################
run.sliding.windows.nnet <- function(sliding.window.size, validate=FALSE, maxit = 100,
                                     hidden.layer.size = 10, decay = 0.4, skip.hidden = FALSE)
{
   nnet.model <- NULL
   nnet.weights <- NULL
   
   ind.windows <- get.windows.indices.list(sliding.window.size)
   
   if (validate) {
      my.test.data <- validate.data
      my.test.labels <- validate.labels
      my.mode <- "VALIDATE"
   } else { 
      my.test.data <- test.data
      my.test.labels <- test.labels
      my.mode <- "  TEST  "
   }
   
   train.acc <- list()
   acc <- list()
   earlier.data <- c()
   earlier.class <- factor(levels=levels.binary)
   earlier.num <- 0
   for (ind in ind.windows) {
      window.data <- rbind(earlier.data, train.data[ind,])
      window.class <- factor(c(as.character(earlier.class),
                               as.character(train.labels[ind])),
                             levels=levels.binary)
      
      # Check if the classes are sufficiently balanced in this sliding window
      unbalanced.classes <- FALSE
      if (count.uniques(as.numeric(window.class)) == 1) {
         unbalanced.classes <- TRUE
      } else {
         class.0.num <- table(window.class)[[1]]
         class.1.num <- table(window.class)[[2]]
         if (table(window.class)[[1]] < 2 || table(window.class)[[2]] < 2) {
            unbalanced.classes <- TRUE
         }
      }
      
      if(unbalanced.classes == TRUE) { 
         # Skip sliding window because it only contains examples from the same class
         # Or because the classes are too unbalanced
         # Save first example to be used in the next sliding window
         earlier.num <- earlier.num + 1
         earlier.data <- rbind(earlier.data, window.data[earlier.num,])
         earlier.class[earlier.num] <- window.class[earlier.num]
         next
      } else {
         earlier.data <- c()
         earlier.class <- factor(levels=levels.binary)
         earlier.num <- 0
      }
      
      # Construct model
      if (is.null(nnet.model) | is.null(nnet.weights)) {
         invisible(capture.output(nnet.model <- nnet(window.class ~ ., data=window.data,
                                                     skip = skip.hidden,
                                                     MaxNWts=5000, size=hidden.layer.size,
                                                     maxit=maxit, decay=decay)))
         nnet.weights <- nnet.model$wts
      } else {
         invisible(capture.output(nnet.model <- nnet(window.class ~ ., data=window.data,
                                                     Wts=nnet.weights,
                                                     skip = skip.hidden,
                                                     MaxNWts=5000, size=hidden.layer.size,
                                                     maxit=maxit, decay=decay)))
         nnet.weights <- nnet.model$wts
      }

      pred.nnet <- factor(round(predict(nnet.model, my.test.data)), levels=levels.binary)
      cm.nnet <- confusionMatrix(pred.nnet, my.test.labels)
      #cat(paste("Neural Net Accuracy =", cm.nnet$overall[[1]], "\n"))
      train.acc[[length(train.acc)+1]] <- confusionMatrix(factor(round(predict(nnet.model, window.data)),
                                                                 levels=levels.binary),
                                                          window.class)$overall[[1]]
      acc[[length(acc)+1]] <- cm.nnet$overall[[1]]

   }
   cat(paste0("Stream Neural Net Classification (sliding.window.size = ", sliding.window.size, " days)\n"))
   cat(paste("Mean TRAINING data accuracy                =", mean(unlist(train.acc)), "\n"))
   cat(paste("Mean", my.mode ,"accuracy                     =", mean(unlist(acc)), "\n"))
   
   # Predict testing dataset
   pred.test <- predict(nnet.model, my.test.data)
   cm.streaming.nnet<- confusionMatrix(factor(round(pred.test), levels=levels.binary), my.test.labels)
   cat(paste("Streaming Neural Network", my.mode, "Accuracy =", cm.streaming.nnet$overall[[1]], "\n"))
   
   # Return mean training accuracy and streaming neural net test accuracy
   c(mean(unlist(train.acc)), cm.streaming.nnet$overall[[1]])
}


#################################################################
# Definition of Ensemble method with Sliding Windows Neural Net
# The weights are passed from one sliding window to the other,
# but only to initialize the next neural network and help training.
# A different neural network is produced for each sliding window.
#################################################################

pred.ens.nnet <- function (ens.nnet, test)
{
  votes.class0 <- c()
  for (model in tail(ens.nnet, global.setup.K)) {
    pred <- predict(model, test)
    votes.class0 <- cbind(votes.class0, round(pred))
  }
  factor(round(unname(rowMeans(votes.class0))), levels=levels.binary)
}

compute.gram.matrix <- function(A) {
  m <- dim(A)[2]
  
  G <- matrix(NA, m, m)
  for(i in 1:m)
    for(j in 1:m)
      G[i, j] <- A[, i] %*% A[, j]
  
  G
}

build.error.table <- function (ens.nnet, test, labels)
{
  votes.class0 <- c()
  for (model in tail(ens.nnet, global.setup.K)) {
    pred <- predict(model, test)
    votes.class0 <- cbind(votes.class0, round(pred))
  }

  apply(votes.class0, 2, function(col) { (col != labels)*1 })
}

##############################################################################
# Note: It is not recommended to go below 10 for 'sliding.window.size' when
# using the Dodgers Loop Dataset, as the sliding windows will frequently
# contain only 1 class example or be very unbalanced
##############################################################################
run.sliding.windows.ens.nnet <- function(sliding.window.size, validate=FALSE, maxit=100,
                                     hidden.layer.size = 10, decay = 0.4, skip.hidden = FALSE,
                                     pass.previous.weights = TRUE)
{
   nnet.model <- NULL
   nnet.prev.weights <- NULL
   
   ind.windows <- get.windows.indices.list(sliding.window.size)
   
   if (validate) {
      my.test.data <- validate.data
      my.test.labels <- validate.labels
      my.mode <- "VALIDATE"
   } else { 
      my.test.data <- test.data
      my.test.labels <- test.labels
      my.mode <- "  TEST  "
   }
   
   train.acc <- list()
   acc <- list()
   ens.nnet <- list()
   earlier.data <- c()
   earlier.class <- factor(levels=levels.binary)
   earlier.num <- 0
   for (ind in ind.windows) {
      window.data <- rbind(earlier.data, train.data[ind,])
      window.class <- factor(c(as.character(earlier.class),
                               as.character(train.labels[ind])),
                             levels=levels.binary)
      
      # Check if the classes are sufficiently balanced in this sliding window
      unbalanced.classes <- FALSE
      if (count.uniques(as.numeric(window.class)) == 1) {
         unbalanced.classes <- TRUE
      } else {
         class.0.num <- table(window.class)[[1]]
         class.1.num <- table(window.class)[[2]]
         if (table(window.class)[[1]] < 2 || table(window.class)[[2]] < 2) {
            unbalanced.classes <- TRUE
         }
      }
          
      if(unbalanced.classes == TRUE) { 
         # Skip sliding window because it only contains examples from the same class
         # Or because the classes are too unbalanced
         # Save first example to be used in the next sliding window
         earlier.num <- earlier.num + 1
         earlier.data <- rbind(earlier.data, window.data[earlier.num,])
         earlier.class[earlier.num] <- window.class[earlier.num]
         next
      } else {
         earlier.data <- c()
         earlier.class <- factor(levels=levels.binary)
         earlier.num <- 0
      }
      
      # Construct model
      if (pass.previous.weights == FALSE | 
             is.null(nnet.model) | is.null(nnet.prev.weights)) {
         invisible(capture.output(nnet.model <- nnet(window.class ~ ., data=window.data,
                                                     skip = skip.hidden,
                                                     MaxNWts=5000, size=hidden.layer.size, maxit=maxit,
                                                     decay=decay)))
         nnet.prev.weights <- nnet.model$wts
      } else {
         invisible(capture.output(nnet.model <- nnet(window.class ~ ., data=window.data,
                                                     Wts=nnet.prev.weights,
                                                     skip = skip.hidden,
                                                     MaxNWts=5000, size=hidden.layer.size, maxit=maxit,
                                                     decay=decay)))
         nnet.prev.weights <- nnet.model$wts
      }
      
      ens.nnet[[length(ens.nnet)+1]] <- nnet.model
      pred.nnet <- factor(round(predict(nnet.model, my.test.data)), levels=levels.binary)
      cm.nnet <- confusionMatrix(pred.nnet, my.test.labels)
      #cat(paste("Neural Net Accuracy =", cm.nnet$overall[[1]], "\n"))
      train.acc[[length(train.acc)+1]] <- confusionMatrix(factor(round(predict(nnet.model, window.data)),
                                                                 levels=levels.binary),
                                                          window.class)$overall[[1]]
      acc[[length(acc)+1]] <- cm.nnet$overall[[1]]
   }
   cat(paste0("Stream Neural Net Classification (sliding.window.size = ", sliding.window.size, " days)\n"))
   cat(paste("Mean TRAINING data accuracy                   =", mean(unlist(train.acc)), "\n"))
   cat(paste("Mean", my.mode ,"accuracy                        =", mean(unlist(acc)), "\n"))
   
   # Predict testing dataset
   pred.test <- pred.ens.nnet(ens.nnet, my.test.data)
   err.table <<- build.error.table(ens.nnet, my.test.data, my.test.labels)
   cm.ens.nnet<- confusionMatrix(pred.test, my.test.labels)
   cat(paste("Ensemble Method Neural Nets", my.mode, "Accuracy =", cm.ens.nnet$overall[[1]], "\n"))
   
   # Return mean training accuracy and ensemble method test accuracy
   c(mean(unlist(train.acc)), cm.ens.nnet$overall[[1]])
}

# # Run some Streaming Neural Net experiments
# setup_test_train(d.byday.30min, 90, 30, discard.bad.rows=TRUE)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=2, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=5, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=10, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=15, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=20, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=25, decay=0.4)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=30, decay=0.4)
# 
# print("NOTE: At this point, the results above indicate that our neural network is clearly
# overfitting on the training data. Indeed, we get approximately 99.6% accuracy on the training
# data. This explains the failure to generalize and the underwhelming results on the test dataset.
# It is my opinion that training on any one single sliding window is no worse than training by
# updating the weights successfully on all sliding windows, because of this.")
# 
# # Misc experiments
# setup_test_train(d.byday, 90, 30, discard.bad.rows=TRUE)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=5, decay=0.4)
# setup_test_train(d.byday.1hour, 30, 30, discard.bad.rows=TRUE, use.weekend.feature=FALSE)
# run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=2, decay=0.4)
# 
# # Try larger decays
# setup_test_train(d.byday.1hour, 30, 30, discard.bad.rows=TRUE, use.weekend.feature=FALSE)
# for (decay in c(0.0001, 0.001, 0.01, 0.1, 0.4, 1, 2, 3, 5, 10, 20)) {
#    cat(paste0("Decay = ", decay, "\n"))
#    run.sliding.windows.nnet(30, validate=FALSE, hidden.layer.size=5, decay=decay)
# }
# 
# setup_test_train(d.byday.1hour, 90, 60, discard.bad.rows=TRUE, use.weekend.feature=FALSE, shuffle.rows=TRUE)
# run.sliding.windows.nnet(20, validate=FALSE, hidden.layer.size=10, decay=1.01, skip.hidden=FALSE)
