# Description:
# Code for Sliding Ensemble Neural Network.
# This contains a function "senn" to build/train the model
# (incrementally) on a streaming dataset.
# There is also a function "predict" to predict unlabeled data
# based on a "senn" model.

senn <- function(formula,
                 data,
                 windows.size,
                 step.length,
                 ensemble.max.size,
                 ...) {
  # Train incrementally a Sliding Ensemble neural Network (senn)
  # model for a streaming dataset.
  #
  # Args:
  #   windows.size: This is the size of the sliding windows (parameter L), i.e.
  #                 the number of rows per sliding window.
  #   step.length: The length of the step (parameter d) 
  #   ensemble.max.size: The maximum size of the ensemble (parameter C)
  #   ...: Arguments to pass to the underlying neural network models.
  # Returns:
  #   A "senn" model
  
  
}

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
  cm.ens.nnet<- confusionMatrix(pred.test, my.test.labels)
  cat(paste("Ensemble Method Neural Nets", my.mode, "Accuracy =", cm.ens.nnet$overall[[1]], "\n"))
  
  # Return mean training accuracy and ensemble method test accuracy
  c(mean(unlist(train.acc)), cm.ens.nnet$overall[[1]])
}
