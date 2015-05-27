work.dir <- "~/h/proj/ieee-stream-mining/code"

# Create 'figures' and 'objects' subdirectories if they don't exist
setwd(work.dir)
dir.create("figures", showWarnings = FALSE)
dir.create("objects", showWarnings = FALSE)

library(xtable)
library(nnet)
library(caret)
library(randomForest)
library(rpart)
library(tree)
library(e1071)
library(class)

# Global variable definitions
save.to.pdf <- TRUE
na.value <- -1
na.count <- 2903

# Citation Request:  
# These loop sensor measurements were obtained from the Freeway Performance Measurement System (PeMS),
# "[http://pems.dot.ca.gov/]" Please include this citation if you plan to use this database.
filename.dodgers.data <- "data/Dodgers.data"
filename.dodgers.events <- "data/Dodgers.events"


# This function calls 'pdf(...)' if 'save.to.pdf' is TRUE,
# otherwise it just calls 'dev.new()'
prep.out <- function(...)
{
   if (save.to.pdf)
      pdf(...)
   else
      dev.new()
}

as.myDate <- function(date)
{
   strptime(paste(date, time), format="%m/%d/%y", tz="UTC")
}
paste.date.time <- function(date, time)
{
   strptime(paste(date, time), format="%m/%d/%y %H:%M:%S", tz="UTC")
}
paste.date.time.interval <- function(date, time.begin, time.end)
{
   t.b <- strptime(paste(date, time.begin), format="%m/%d/%y %H:%M:%S", tz="UTC")
   t.e <- strptime(paste(date, time.end), format="%m/%d/%y %H:%M:%S", tz="UTC")
   c(t.b, t.e)
}
extract.seconds <- function(datetime)
{
   sapply(strsplit(strftime(datetime, "%H:%M:%S", tz="UTC"),":"),
          function(x) {
             x <- as.numeric(x)
             x[1]*60*60+x[2]*60+x[3]
          }
   )
}
count.uniques <- function(x)
{
   length(levels(as.factor(x)))
}

# Dodgers.data file format
# 1.  Date: MM/DD/YY
# 2.  Begin event time: HH:MM:SS (military) 
# 3.  End event time: HH:MM:SS (military)
#

setClass("myDatetime")
setAs("character", "myDatetime", function (from) strptime(from, "%m/%d/%Y %H:%M", tz="UTC"))

d.dodgers <-read.table(filename.dodgers.data, header = FALSE, sep=",",
                       col.names = c("datetime", "count"),
                       na.strings = "-1",
                       colClasses = c("myDatetime", "integer"))
# Print summary of data
(global.first.day <- min(as.Date(d.dodgers$datetime)))
(global.last.day <- max(as.Date(d.dodgers$datetime)))
global.days <- as.Date(global.first.day:global.last.day, origin="1970-01-01", tz="UTC")
(global.num.days <- length(global.days))

# Dodgers.events format
# 1.  Date: MM/DD/YY
# 2.  Begin event time: HH:MM:SS (military) 
# 3.  End event time: HH:MM:SS (military)
# 4.  Game attendance
# 5.  Away team
# 6.  W/L score
e.dodgers.raw <-read.table(filename.dodgers.events, header = FALSE, sep=",",
                           col.names = c("date", "event.time.begin", "event.time.end", "attendance", "away.team", "score"),
                           colClasses = c("character", "character", "character", "integer", "character", "character"))

e.dodgers <- data.frame(event.time.begin=paste.date.time(e.dodgers.raw$date, e.dodgers.raw$event.time.begin),
                        event.time.end=paste.date.time(e.dodgers.raw$date, e.dodgers.raw$event.time.end),
                        attendance=e.dodgers.raw$attendance,
                        away.team=e.dodgers.raw$away.team,
                        score=e.dodgers.raw$score)


parse.day <- function(days)
{
   d <- strptime(days, format="%m/%d/%y", tz="UTC")
   if (any(is.na(d)))
      d <- as.Date(days, tz="UTC")
   return(d)
}
get.day.index <- function(days)
{
   if (is.numeric(days)) {
      if (days <= 0) 
         stop("Day must be a positive integer or a date string.")
      return(days)
   }
   pd <- parse.day(days)
   return(as.integer(pd - global.first.day + 1))
}
plot.interval <- function(data, start, end)
{
   if(!is.numeric(start) || !is.numeric(end)) {
      num <- nrow(data)
      if (start > end)
         stop("Please make sure 'start' < 'end'")
      if (data[num, "datetime"] < start)
         stop("Invalid 'start' value: beyond last date")
      if (data[1, "datetime"] > end)
         stop("Invalid 'end' value: before first date")
      dt <- force(data$datetime)
      ind.min <- {for(i in 1:num) { if(dt[i] > start) break }; i}
      ind.max <- {for(i in num:ind.min) { if(dt[i] < end) break }; i}
      ind <- ind.min:ind.max
   } else {
      ind <- start:end
   }
   
   plot(data[ind,"datetime"], data[ind,"count"], type="l", xaxs="i")
}
plot.histogram <- function(data, start, end)
{
   if(!is.numeric(start) || !is.numeric(end)) {
      num <- nrow(data)
      if (start > end)
         stop("Please make sure 'start' < 'end'")
      if (data[num, "datetime"] < start)
         stop("Invalid 'start' value: beyond last date")
      if (data[1, "datetime"] > end)
         stop("Invalid 'end' value: before first date")
      dt <- force(data$datetime)
      ind.min <- {for(i in 1:num) { if(dt[i] > start) break }; i}
      ind.max <- {for(i in num:ind.min) { if(dt[i] < end) break }; i}
      ind <- ind.min:ind.max
   } else {
      ind <- start:end
   }
   
   total.day.count <- sum(data[ind, "count"])
   Z <- c()
   for (x in ind) {
      Z <- c(Z, rep(as.numeric(data[x, "datetime"]), data[x, "count"]))
   }
   hist(Z, breaks=breaks)
}
plot.all.events <- function(events, col.begin="blue", col.end="red", lty="longdash", lwd=1.5, ...)
{
   abline(v=as.numeric(events$event.time.begin), col=col.begin, lty=lty, lwd=lwd, ...)
   abline(v=as.numeric(events$event.time.end), col=col.end, lty=lty, lwd=lwd, ...)
}

days.with.event <- function()
{
   events <- as.Date(e.dodgers$event.time.end, tz="UTC")
   (!is.na(match(global.days, events)))*1
}

get.event.times <- function()
{
   events <- as.Date(e.dodgers$event.time.end, tz="UTC")
   m <- match(global.days, events)
   events.begin <- rep(as.POSIXct(NA), global.num.days)
   events.end <- rep(as.POSIXct(NA), global.num.days)
   events.begin[which(!is.na(match(global.days, events)))] <- e.dodgers$event.time.begin
   events.end[which(!is.na(match(global.days, events)))] <- e.dodgers$event.time.end
   attr(events.begin, "tzone") <- "UTC"
   attr(events.end, "tzone") <- "UTC"
   data.frame(event.time.begin=events.begin, event.time.end=events.end)
}

day.has.event <- function(day)
{
   if (!is.numeric(day)) {
      begin.day <- as.Date(d.dodgers[(day-1)*288+1, "datetime"], tz="UTC")
      begin.day.index <- as.integer(begin.day - global.first.day) + 1
   } else if (day <= 0) {
      stop("Day must a positive integer or a string.")
   } else {
      begin.day.index <- day
   }
   days.with.event()[begin.day.index]
}

is.weekend <- function(days)
{
   wd <- weekdays(days)
   wd == "Saturday" | wd == "Sunday"
}

############################
# Data pre-processing
############################

# Split the data into days (288 rows make a day, at 5 min. per interval)
list.d.days <- lapply(1:global.num.days,
                      function(day) d.dodgers[((day-1)*288+1):(day*288),"count"])
# Convert the list of vectors into a matrix
matrix.d.days <- ts(do.call(rbind, list.d.days))


# Create data.frame where each row is a day
d.byday.raw <- data.frame(day=global.days,
                          is.weekend=is.weekend(global.days),
                          data=I(list.d.days),
                          class=days.with.event(),
                          get.event.times(),
                          check.rows=FALSE)

# Check how many missing days there are per row
days.num.NA <- unlist(lapply(1:global.num.days, function(day) sum(is.na(d.byday.raw$data[[day]]))))

# Mark all rows with more than 10 missing values
days.discard <- which(days.num.NA > 10)

# Remove these days
#d.byday.raw <- d.byday.raw[-days.discard, ]
#row.names(d.byday.raw) <- NULL

# Set a global index of remaining days
#global.d.days.index <- (1:global.num.days)[is.na(match(1:global.num.days, days.discard))]

# Imput missing data (replace NA with 0)
sub.na.by <- function (data, new.na.value)
{
   I(lapply(1:length(data), function(day) {
      x <- data[[day]]
      x[is.na(x)] <- new.na.value
      x }))
}

# Replace NA values by 0
d.byday <- d.byday.raw
d.byday$data <- sub.na.by(d.byday.raw$data, 0)

# Group data into 1 hour chunks
d.byday.1hour <- d.byday
for (d in 1:nrow(d.byday)) {
   day.index <- d
   T <- rep(0, 24)
   for (x in 1:24) {
      ind <- ((x-1)*12+1):(x*12)
      T[x] <- sum(d.byday[[day.index, "data"]][ind])
   }
   d.byday.1hour[[day.index, "data"]] <- T
}

# Group data into 30 minutes chunks
d.byday.30min <- d.byday
for (d in 1:nrow(d.byday)) {
   day.index <- d
   T <- rep(0, 48)
   for (x in 1:48) {
      ind <- ((x-1)*6+1):(x*6)
      T[x] <- sum(d.byday[[day.index, "data"]][ind])
   }
   d.byday.30min[[day.index, "data"]] <- T
}

######################################################
# DATA PRE-PROCESSING PLOTTING SECTION STARTS HERE
######################################################


########################
# Plot helper functions
########################
plot.lines <- function(data)
{
   lines((1:288 * 5)/60, data, type="l", lwd=3.0,
         col=rgb(30,144,255,120,maxColorValue=255))
}


plot.byday <- function(day)
{
   index <- get.day.index(day)
   plot((1:288 * 5)/60, d.byday$data[[day]], type="l", xaxs="i", xaxt="n",
        xlab = "Time (hours)", ylab = "Cars count")
   axis(1, at=seq(0,24,2), las=2)
   title(main=paste(global.days[day], " (", weekdays(global.days[day]),"), Day #",
                    day, ", Class = ", day.has.event(day)*1, sep=""))
   if (d.byday[day, "class"] == 1) {
      start <- extract.seconds(d.byday$event.time.begin[[day]])/3600
      end <- extract.seconds(d.byday$event.time.end[[day]])/3600
      abline(v=start, col="blue", lty="longdash", lwd=1.5)
      abline(v=end, col="red", lty="longdash", lwd=1.5)
   }
}

plot.hist.byday <- function(day, breaks)
{
   index <- get.day.index(day)
   indices <- 1:288
   Z <- c()
   for (x in indices) {
      Z <- c(Z, rep(x, d.byday[day,"data"][[1]][x]))
   }
   hist(Z*5/60,
        breaks=breaks, main="", xaxt="n", xlab = "Time (hours)", ylab = "Cars count")
   axis(1, at=seq(0,24,2), las=2)
   title(main=paste(global.days[day], " (", weekdays(global.days[day]),"), Day #",
                    day, ", Class = ", day.has.event(day)*1, sep=""))
   if (d.byday[day, "class"] == 1) {
      start <- extract.seconds(d.byday$event.time.begin[[day]])/3600
      end <- extract.seconds(d.byday$event.time.end[[day]])/3600
      abline(v=start, col="blue", lty="longdash", lwd=1.5)
      abline(v=end, col="red", lty="longdash", lwd=1.5)
   }   
}

print.all.weeks <- function()
{
   all.indices <- list()
   apply(matrix(c(1:global.num.days, 1:7), ncol=14, byrow=TRUE), 1, function(x) all.indices[[length(all.indices)+1]] <<- x)
   for (days in all.indices) {
      oldpar <- par(mfcol=c(7,2), mar=c(2,5,1.5,2))
      for (day in days) {
         plot.byday(day)
         plot.all.events(e.dodgers)
         title(main=paste(global.days[day], " (", weekdays(global.days[day]),"), Day #", day, ", Class = ", day.has.event(day)*1, sep=""))
      }
      par(oldpar)
      cat ("Press [enter] to continue")
      line <- readline()
   }
}

################################
# Start plotting graphs
################################

# Plot a representative week

prep.out("figures/typical-week.pdf", width=7, height=3.5)
oldpar <- par(mar=c(6,0,0,0), oma=c(3,4,1,1.5))
indices <- (288*22+1):(288*29)
plot(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
     type="l", xaxs="i", lwd=0, col="WHITE",
     xlab="", ylab="", xaxt="n")
plot.all.events(e.dodgers, lwd=1.5, lty=1,
                col.begin=rgb(0,100,255,255,maxColorValue=255),
                col.end=rgb(255,0,00,255,maxColorValue=255))
#points(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"], pch=16,
#       col=rgb(30,144,255,50,maxColorValue=255))
lines(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
      type="l", xaxs="i", lwd=1)
axis(1, las=2, at = c(as.numeric(d.dodgers[seq(indices[[1]], indices[[1]]+6*288, 288), "datetime"]),
                      as.numeric(d.dodgers[288*29-1, "datetime"])),
     labels = paste0(weekdays(d.dodgers[seq(indices[[1]], indices[[1]]+7*288, 288), "datetime"]),
                     "\n",
                     format(d.dodgers[seq(indices[[1]], indices[[1]]+7*288, 288), "datetime"], 
                            format="%d %B %Y"),
                     ""))
title(xlab = "Day of the week",
      outer = TRUE, line = 1)
title(ylab = "Car counts per 5 min.",
      outer = TRUE, line = 2.5)
par(oldpar)
dev.off()


# Plot 1 week, day by day
prep.out("figures/two-weeks-raw.pdf", width=7, height=8.5)
oldpar <- par(mfcol=c(7,2), mar=c(2,5,1.5,2))
for (day in 9:22) {
   plot.byday(day)
   plot.all.events(e.dodgers, lwd=3)
}
par(oldpar)
dev.off()

# Plot 2 weeks, day by day, in histogram format, with 30 min breaks
# We are doing some data reduction here... :)
prep.out("figures/two-weeks-reduced-dimensionality.pdf", width=7, height=8.5)
oldpar <- par(mfcol=c(7,2), mar=c(2,5,1.5,2))
for (day in 9:22) {
   plot.hist.byday(day, 48)
   plot.all.events(e.dodgers, lwd=2)
}
par(oldpar)
dev.off()

# Plot 2 weeks, day by day, in histogram format, with 1 hour breaks
# We are doing some data reduction here... :)
prep.out("figures/two-weeks-reduced-dimensionality-1-hour.pdf", width=7, height=8.5)
oldpar <- par(mfcol=c(7,2), mar=c(2,5,1.5,2))
for (day in 9:22) {
   plot.hist.byday(day, 24)
   plot.all.events(e.dodgers, lwd=2)
}
par(oldpar)
dev.off()


# Plot first 7 days
# We are doing some data reduction here... :)
prep.out("figures/first-7-days.pdf", width=7, height=8.5)
oldpar <- par(mfcol=c(7,1), mar=c(2,5,1.5,2))
for (day in 1:7) {
   plot.byday(day)
   plot.all.events(e.dodgers, lwd=2)
}
par(oldpar)
dev.off()



# Plot same day with 288, 48 and 24 features
# We are doing some data reduction here... :)
prep.out("figures/compare-binning.pdf", width=7, height=4)
oldpar <- par(mfcol=c(3,1), mar=c(2,5,2.5,2))
day <- 17
plot.byday(day)
plot.all.events(e.dodgers, lwd=4)
plot.hist.byday(day, 48)
plot.all.events(e.dodgers, lwd=4)
plot.hist.byday(day, 24)
plot.all.events(e.dodgers, lwd=4)
par(oldpar)
dev.off()

#########################################################
# "BACKBONE" CODE FOR SLIDING WINDOWS, SLIDING FOREST
# SLIDING MINI-BATCH NEURAL NETWORKS,
# SLIDING ENSEMBLE NEURAL NETWORKS, ETC.
#########################################################

#############################
# Start training classifiers
#############################
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
   cm.ens.nnet <- confusionMatrix(pred.test, my.test.labels)
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

margins <-c(4, 4, 0.8, 0.5)
std.width <- 7
std.height <- 2.8

std.maxit <- 50
std.h.size <- 4

###########################
# Misc. helper functions
###########################
xtable.custom <- function(df, rownames=NULL, colnames=NULL,
                          include.rownames=TRUE, include.colnames=TRUE,
                          ...)
{
   df.copy <- df
   if (!is.null(colnames)) { 
      colnames(df.copy) <- colnames
   }
   if (!is.null(rownames)) {
      rownames(df.copy) <- rownames
   }
   xres <- xtable(df.copy, ...)
   print(xres, include.rownames=include.rownames, include.colnames=include.colnames)
}

with.loop <- function(values, FUN, ...)
{
   res <- c()
   for (v in values) {
      seed <- v
      time <- system.time(out <- FUN(v, ...))
      res <- rbind(res, c(v, out, time[[3]], ...))
   }
   res
}

############################################
# CLASSIFICATION PERFORMANCE EXPERIMENTS
############################################

# Run Ensemble of Trees on Sliding Windows
# Vary the sliding window sizes

if (file.exists("objects/exp.trees.ensemble")) {
   exp.trees.ensemble <- readRDS("objects/exp.trees.ensemble")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.ens.trees)
   exp.trees.ensemble <- c()
   exp.trees.ensemble$setup <- get.current.setup()
   exp.trees.ensemble$results <- data.frame(windows.size=r[,1],
                                            train.accuracy=r[,2],
                                            test.accuracy=r[,3],
                                            time=r[,4])
   saveRDS(exp.trees.ensemble, file="objects/exp.trees.ensemble")
}

prep.out("figures/results-trees-ensemble.pdf", width=std.width, height=std.height)
with(exp.trees.ensemble$results, {
   par(mar=margins)
   plot(train.accuracy ~ windows.size, col=2, type="o", pch=15, ylim=range(c(0.4,1)),
        ylab="Accuracy", xlab="Sliding windows size")
   lines(test.accuracy ~ windows.size, type="o", col=3, pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Train Accuracy", "Test Accuracy"), col=c(2,3), pch=15, bg="white")
})
dev.off()

# Run Ensemble Neural Nets on Sliding Windows
# Vary the sliding window sizes

if (file.exists("objects/exp.nnet.ensemble")) {
   exp.nnet.ensemble <- readRDS("objects/exp.nnet.ensemble")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.ens.nnet,
                  hidden.layer.size=std.h.size, maxit=std.maxit, decay=0.1,
                  pass.previous.weights=TRUE)
   exp.nnet.ensemble <- c()
   exp.nnet.ensemble$setup <- get.current.setup()
   exp.nnet.ensemble$results <- data.frame(windows.size=r[,1],
                                           train.accuracy=r[,2],
                                           test.accuracy=r[,3],
                                           time=r[,4],
                                           hidden.layer.size=r[,5],
                                           maxit=r[,6],
                                           decay=r[,7],
                                           pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ensemble, file="objects/exp.nnet.ensemble")
}

prep.out("figures/results-nnet-ensemble.pdf", width=std.width, height=std.height)
with(exp.nnet.ensemble$results, {
   par(mar=margins)
   plot(train.accuracy ~ windows.size, col=2, type="o", pch=15, ylim=range(c(0.75,1)),
        ylab="Accuracy", xlab="Sliding windows size")
   lines(test.accuracy ~ windows.size, type="o", col=3, pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Train Accuracy", "Test Accuracy"), col=c(2,3), pch=15, bg="white")
})
dev.off()


# Run Ensemble Neural Nets on Sliding Windows
# DO NOT pass weights from one window to the next

if (file.exists("objects/exp.nnet.ensemble.dnpw")) {
   exp.nnet.ensemble.dnpw <- readRDS("objects/exp.nnet.ensemble.dnpw")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.ens.nnet,
                  hidden.layer.size=std.h.size, maxit=std.maxit, decay=0.1,
                  pass.previous.weights=FALSE)
   exp.nnet.ensemble.dnpw <- c()
   exp.nnet.ensemble.dnpw$setup <- get.current.setup()
   exp.nnet.ensemble.dnpw$results <- data.frame(windows.size=r[,1],
                                                train.accuracy=r[,2],
                                                test.accuracy=r[,3],
                                                time=r[,4],
                                                hidden.layer.size=r[,5],
                                                maxit=r[,6],
                                                decay=r[,7],
                                                pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ensemble.dnpw, file="objects/exp.nnet.ensemble.dnpw")
}

prep.out("figures/results-nnet-ensemble-dnpw.pdf", width=std.width, height=std.height)
with(exp.nnet.ensemble.dnpw$results, {
   par(mar=margins)
   plot(train.accuracy ~ windows.size, col=2, type="o", pch=15, ylim=range(c(0.75,1)),
        ylab="Accuracy", xlab="Sliding windows size")
   lines(test.accuracy ~ windows.size, type="o", col=3, pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Train Accuracy", "Test Accuracy"), col=c(2,3), pch=15, bg="white")
})
dev.off()


# Run Ensemble Neural Nets on Sliding Windows
# Vary the number of maximum iterations
if (file.exists("objects/exp.nnet.ens.maxit.pw")) {
   exp.nnet.ens.maxit.pw <- readRDS("objects/exp.nnet.ens.maxit.pw")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- c()
   for (maxit in seq(2,100,2)) {
      win.size <- 20
      seed <- maxit
      set.seed(seed)
      time <- system.time(out <- run.sliding.windows.ens.nnet(win.size, hidden.layer.size=std.h.size,
                                                              maxit=maxit, decay=0.1,
                                                              pass.previous.weights=TRUE))
      r <- rbind(r, c(win.size, out, time[[3]],
                      hidden.layer.size=std.h.size, maxit=maxit, decay=0.1,
                      pass.previous.weights=TRUE))
   }
   exp.nnet.ens.maxit.pw <- c()
   exp.nnet.ens.maxit.pw$setup <- get.current.setup()
   exp.nnet.ens.maxit.pw$results <- data.frame(windows.size=r[,1],
                                               train.accuracy=r[,2],
                                               test.accuracy=r[,3],
                                               time=r[,4],
                                               hidden.layer.size=r[,5],
                                               maxit=r[,6],
                                               decay=r[,7],
                                               pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ens.maxit.pw, file="objects/exp.nnet.ens.maxit.pw")
}

if (file.exists("objects/exp.nnet.ens.maxit.dnpw")) {
   exp.nnet.ens.maxit.dnpw <- readRDS("objects/exp.nnet.ens.maxit.dnpw")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- c()
   for (maxit in seq(2,100,2)) {
      win.size <- 20
      seed <- maxit
      set.seed(seed)
      time <- system.time(out <- run.sliding.windows.ens.nnet(win.size, hidden.layer.size=std.h.size,
                                                              maxit=maxit, decay=0.1,
                                                              pass.previous.weights=FALSE))
      r <- rbind(r, c(win.size, out, time[[3]],
                      hidden.layer.size=std.h.size, maxit=maxit, decay=0.1,
                      pass.previous.weights=FALSE))
   }
   exp.nnet.ens.maxit.dnpw <- c()
   exp.nnet.ens.maxit.dnpw$setup <- get.current.setup()
   exp.nnet.ens.maxit.dnpw$results <- data.frame(windows.size=r[,1],
                                                 train.accuracy=r[,2],
                                                 test.accuracy=r[,3],
                                                 time=r[,4],
                                                 hidden.layer.size=r[,5],
                                                 maxit=r[,6],
                                                 decay=r[,7],
                                                 pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ens.maxit.dnpw, file="objects/exp.nnet.ens.maxit.dnpw")
}

prep.out("figures/results-nnet-ens-maxit.pdf", width=std.width, height=std.height)
par(mar=margins)
plot(exp.nnet.ens.maxit.pw$results$train.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
     col=4, type="o", pch=15, ylim=range(c(0.5,1.0)),
     ylab="Accuracy", xlab="Maximum number of iterations (maxit)")
lines(exp.nnet.ens.maxit.pw$results$test.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=5, pch=15)
lines(exp.nnet.ens.maxit.dnpw$results$train.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=8, pch=15)
lines(exp.nnet.ens.maxit.dnpw$results$test.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=9, pch=15)
grid(col="lightblue4")
legend("bottomright", c("Pass weights (training set)", "Pass weights (testing set)",
                        "Do not pass weights (training set)", "Do not pass weights (testing set)"),
       col=c(4,5,8,9), pch=15, bg="white")
dev.off()


# Run Mini-batch (i.e. sequential) Neural Nets on Sliding Windows
# Vary the sliding window sizes

if (file.exists("objects/exp.nnet.minibatch")) {
   exp.nnet.minibatch <- readRDS("objects/exp.nnet.minibatch")
} else {
   setup.test.train(d.byday.1hour, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.nnet,
                  hidden.layer.size=std.h.size, maxit=std.maxit, decay=0.1)
   exp.nnet.minibatch <- c()
   exp.nnet.minibatch$setup <- get.current.setup()
   exp.nnet.minibatch$results <- data.frame(windows.size=r[,1],
                                            train.accuracy=r[,2],
                                            test.accuracy=r[,3],
                                            time=r[,4],
                                            hidden.layer.size=r[,5],
                                            maxit=r[,6],
                                            decay=r[,7])
   saveRDS(exp.nnet.minibatch, file="objects/exp.nnet.minibatch")
}

prep.out("figures/results-nnet-minibatch.pdf", width=std.width, height=std.height)
with(exp.nnet.minibatch$results, {
   par(mar=margins)
   plot(train.accuracy ~ windows.size, col=2, type="o", pch=15, ylim=range(train.accuracy, test.accuracy),
        ylab="Accuracy", xlab="Sliding windows size")
   lines(test.accuracy ~ windows.size, type="o", col=3, pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Train Accuracy", "Test Accuracy"), col=c(2,3), pch=15, bg="white")
})
dev.off()

# Plot compare minibatch nnet with ens. trees
prep.out("figures/results-compare-trees-minibatch.pdf", width=std.width, height=std.height)
par(mar=margins)
with(exp.nnet.minibatch$results, {
   plot(test.accuracy ~ windows.size, col="orange", type="o", pch=15,
        ylim=range(test.accuracy, exp.trees.ensemble$results$test.accuracy),
        ylab="Accuracy", xlab="Sliding windows size")
})
with(exp.trees.ensemble$results, {
   lines(test.accuracy ~ windows.size, type="o", col="green", pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Sliding Mini-batch Neural Networks", "Sliding Forests"),
          col=c("orange", "green"), pch=15, bg="white")
})
dev.off()
########################################
# COMPARISON OF EXPERIMENTS
########################################
prep.out("figures/results-nnet-compare.pdf", width=std.width, height=std.height)
par(mar=margins)
plot(exp.nnet.ensemble$results$test.accuracy ~ exp.nnet.ensemble$results$windows.size,
     col=4, type="o", pch=15,
     ylim=range(exp.nnet.ensemble$results$test.accuracy, exp.nnet.minibatch$results$test.accuracy),
     ylab="Accuracy", xlab="Sliding windows size")
lines(exp.nnet.minibatch$results$test.accuracy ~ exp.nnet.minibatch$results$windows.size,
      col=6, type="o", pch=15,)
grid(col="lightblue4")
legend("bottomright", c("Sliding Ensemble Neural Networks", "Sliding Mini-batch Neural Networks"), col=c(4,6), pch=15, bg="white")
dev.off()

prep.out("figures/results-ens-methods-compare.pdf", width=std.width, height=std.height)
par(mar=margins)
plot(exp.nnet.ensemble$results$test.accuracy ~ exp.nnet.ensemble$results$windows.size,
     col=4, type="o", pch=15,
     ylim=range(exp.nnet.ensemble$results$test.accuracy,exp.trees.ensemble$results$test.accuracy ),
     ylab="Accuracy", xlab="Sliding windows size")
lines(exp.trees.ensemble$results$test.accuracy ~ exp.trees.ensemble$results$windows.size,
      col=3, type="o", pch=15,)
grid(col="lightblue4")
legend("bottomright", c("Sliding Ensemble Neural Networks", "Sliding Forests"), col=c(4,3), pch=15, bg="white")
dev.off()


prep.out("figures/results-all-methods-compare.pdf", width=std.width, height=std.height)
par(mar=margins)
plot(exp.trees.ensemble$results$test.accuracy ~ exp.trees.ensemble$results$windows.size,
     col=3, type="o", pch=15,
     ylim=range(exp.trees.ensemble$results$test.accuracy,
                exp.nnet.minibatch$results$test.accuracy,
                exp.nnet.ensemble$results$test.accuracy,
                exp.nnet.ensemble.dnpw$results$test.accuracy),
     ylab="Accuracy", xlab="Sliding windows size")
lines(exp.nnet.minibatch$results$test.accuracy ~ exp.nnet.minibatch$results$windows.size,
      col=6, type="o", pch=15,)
lines(exp.nnet.ensemble$results$test.accuracy ~ exp.nnet.ensemble$results$windows.size,
      col=4, type="o", pch=15,)
lines(exp.nnet.ensemble.dnpw$results$test.accuracy ~ exp.nnet.ensemble.dnpw$results$windows.size,
      col=2, type="o", pch=15,)
grid(col="lightblue4")
legend("bottomright",
       c("Sliding Forests",
         "Sliding Mini-batch Neural Networks",
         "Sliding Ensemble Neural Networks",
         "Sliding Ensemble Neural Networks (do not pass weights)"),
       col=c(3,6,4,2), pch=15, bg="white")
dev.off()

# Print summary of comparison of all 3 methods in a LaTeX table
xtable.custom(cbind(exp.nnet.ensemble$results$windows.size,
                    exp.trees.ensemble$results$test.accuracy,
                    exp.nnet.minibatch$results$test.accuracy,
                    exp.nnet.ensemble$results$test.accuracy,
                    exp.nnet.ensemble.dnpw$results$test.accuracy),
              colnames = c("Size of sliding windows", 
                           "Sliding Forests",
                           "Sliding Mini-batch Neural Networks",
                           "Sliding Ensemble Neural Networks",
                           "Sliding Ensemble Neural Networks (do not pass weights)"),
              include.rownames=FALSE, digits=3)
# Print average values
signif(colMeans(cbind(exp.trees.ensemble$results$test.accuracy,
                      exp.nnet.minibatch$results$test.accuracy,
                      exp.nnet.ensemble$results$test.accuracy,
                      exp.nnet.ensemble.dnpw$results$test.accuracy)[6:16,]), 3)

###################################
###################################
###  TODO: COMPARISON WITH VFDT
###################################
###################################


########################################
# TIMING EXPERIMENTS
########################################

# Run Ensemble Neural Nets on Sliding Windows
# Vary the sliding window sizes
if (file.exists("objects/exp.nnet.ens.time.donotpass")) {
   exp.nnet.ens.time.pass <- readRDS("objects/exp.nnet.ens.time.pass")
} else {
   setup.test.train(d.byday, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.ens.nnet,
                  hidden.layer.size=std.h.size, maxit=std.maxit, decay=0.1,
                  pass.previous.weights=TRUE)
   exp.nnet.ens.time.pass <- c()
   exp.nnet.ens.time.pass$setup <- get.current.setup()
   exp.nnet.ens.time.pass$results <- data.frame(windows.size=r[,1],
                                                train.accuracy=r[,2],
                                                test.accuracy=r[,3],
                                                time=r[,4],
                                                hidden.layer.size=r[,5],
                                                maxit=r[,6],
                                                decay=r[,7],
                                                pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ens.time.pass, file="objects/exp.nnet.ens.time.pass")
}
if (file.exists("objects/exp.nnet.ens.time.donotpass")) {
   exp.nnet.ens.time.donotpass <- readRDS("objects/exp.nnet.ens.time.donotpass")
} else {
   # Run Ensemble Neural Nets on Sliding Windows
   # Vary the sliding window sizes
   # DO NOT pass weights to the next window
   setup.test.train(d.byday, 70, 81)
   r <- with.loop(seq(5,50,3), run.sliding.windows.ens.nnet,
                  hidden.layer.size=std.h.size, maxit=std.maxit, decay=0.1,
                  pass.previous.weights=FALSE)
   exp.nnet.ens.time.donotpass <- c()
   exp.nnet.ens.time.donotpass$setup <- get.current.setup()
   exp.nnet.ens.time.donotpass$results <- data.frame(windows.size=r[,1],
                                                     train.accuracy=r[,2],
                                                     test.accuracy=r[,3],
                                                     time=r[,4],
                                                     hidden.layer.size=r[,5],
                                                     maxit=r[,6],
                                                     decay=r[,7],
                                                     pass.previous.weights=r[,8])
   saveRDS(exp.nnet.ens.time.donotpass, file="objects/exp.nnet.ens.time.donotpass")
}

prep.out("figures/results-nnet-ens-timing.pdf", width=std.width, height=std.height)
par(mar=margins)
# Plot 'time ~ windows.size', comparing pass and donotpass experiments
with(exp.nnet.ens.time.pass$results, {
   plot(1000*time/(70-windows.size+1) ~ windows.size, col=4, type="o", pch=15,
        ylim=range(1000*c(time/(70-windows.size+1),
                          exp.nnet.ens.time.donotpass$results$time/(70-exp.nnet.ens.time.donotpass$results$windows.size+1))),
        #ylim=range(exp.nnet.ens.time.pass$results$time, exp.nnet.ens.time.donotpass$results$time),
        ylab="Time / window (milliseconds)", xlab="Sliding windows size")
})
with(exp.nnet.ens.time.donotpass$results, {
   lines(1000*time/(70-windows.size+1) ~ windows.size, type="o", col=5, pch=15)
   grid(col="lightblue4")
   legend("bottomright", c("Pass weights to next window", "Do not pass weights"), col=c(4,5), pch=15, bg="white")
})
dev.off()
# Compute mean of time speedup
mean((exp.nnet.ens.time.donotpass$results$time/(70-exp.nnet.ens.time.donotpass$results$windows.size+1)) /
        (exp.nnet.ens.time.pass$results$time/(70-exp.nnet.ens.time.pass$results$windows.size+1)))
#############################################
# RUN BENCHMARKS
#############################################
if (file.exists("objects/exp.benchmark")) {
   exp.benchmark <- readRDS("objects/exp.benchmark")
} else {
   # Run standard benchmarks 10 times and compute average.
   setup.test.train(d.byday.1hour, 70, 81)
   set.seed(0)
   bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
   df.train <- data.frame(bench$name, bench$train.acc)
   df.test <- data.frame(bench$name, bench$test.acc)
   for (j in 1:9) {
      set.seed(j)
      bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
      df.train <- cbind(df.train, bench$train.acc)
      df.test <- cbind(df.test, bench$test.acc)
   }
   exp.benchmark <- c()
   exp.benchmark$setup <- get.current.setup()
   exp.benchmark$results <- data.frame(name=df.train[,1],
                                       avg.train.acc=rowMeans(df.train[,-1]),
                                       avg.test.acc=rowMeans(df.test[,-1]))
   saveRDS(exp.benchmark, file="objects/exp.benchmark")
}

prep.out("figures/results-benchmark.pdf", width=std.width, height=3.5)
oldpar <- par(xpd=NA, oma=c(6,0,0,0), mar=c(1.1, 4.1, 1, 0.5))
mp <- barplot(xtabs(avg.test.acc ~ name, exp.benchmark$results),
              ylim=c(0,1), las=2,
              ylab= "Test Accuracy", xlab="", col=rainbow(nrow(exp.benchmark$results)))
title(xlab = "Classifier", outer = TRUE, line = 4)
vals <- signif(exp.benchmark$results$avg.test.acc, 3)
text(mp, vals, labels = vals, pos = 3)
par(oldpar)
dev.off()

if (file.exists("objects/exp.benchmark.2")) {
   exp.benchmark.2 <- readRDS("objects/exp.benchmark.2")
} else {
   # Run standard benchmarks 10 times and compute average.
   setup.test.train(d.byday.30min, 70, 81)
   set.seed(0)
   bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
   df.train <- data.frame(bench$name, bench$train.acc)
   df.test <- data.frame(bench$name, bench$test.acc)
   for (j in 1:9) {
      set.seed(j)
      bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
      df.train <- cbind(df.train, bench$train.acc)
      df.test <- cbind(df.test, bench$test.acc)
   }
   exp.benchmark.2 <- c()
   exp.benchmark.2$setup <- get.current.setup()
   exp.benchmark.2$results <- data.frame(name=df.train[,1],
                                         avg.train.acc=rowMeans(df.train[,-1]),
                                         avg.test.acc=rowMeans(df.test[,-1]))
   saveRDS(exp.benchmark.2, file="objects/exp.benchmark.2")
}

prep.out("figures/results-benchmark-2.pdf", width=std.width, height=std.height)
oldpar <- par(xpd=NA, oma=c(6,0,0,0), mar=c(1.1, 4.1, 0.75, 0.5))
mp <- barplot(xtabs(avg.test.acc ~ name, exp.benchmark.2$results),
              ylim=c(0,1), las=2,
              ylab= "Test Accuracy", xlab="", col=rainbow(nrow(exp.benchmark.2$results)))
title(xlab = "Classifier", outer = TRUE, line = 4)
vals <- signif(exp.benchmark.2$results$avg.test.acc, 3)
text(mp, vals, labels = vals, pos = 3)
par(oldpar)
dev.off()


if (file.exists("objects/exp.benchmark.3")) {
   exp.benchmark.3 <- readRDS("objects/exp.benchmark.3")
} else {
   # Run standard benchmarks 10 times and compute average.
   setup.test.train(d.byday, 70, 81)
   set.seed(0)
   bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
   df.train <- data.frame(bench$name, bench$train.acc)
   df.test <- data.frame(bench$name, bench$test.acc)
   for (j in 1:9) {
      set.seed(j)
      bench <- run.benchmarks(train.data, train.labels, test.data, test.labels)
      df.train <- cbind(df.train, bench$train.acc)
      df.test <- cbind(df.test, bench$test.acc)
   }
   exp.benchmark.3 <- c()
   exp.benchmark.3$setup <- get.current.setup()
   exp.benchmark.3$results <- data.frame(name=df.train[,1],
                                         avg.train.acc=rowMeans(df.train[,-1]),
                                         avg.test.acc=rowMeans(df.test[,-1]))
   saveRDS(exp.benchmark.3, file="objects/exp.benchmark.3")
}

prep.out("figures/results-benchmark-3.pdf", width=std.width, height=std.height)
oldpar <- par(xpd=NA, oma=c(6,0,0,0), mar=c(1.1, 4.1, 0.75, 0.5))
mp <- barplot(xtabs(avg.test.acc ~ name, exp.benchmark.3$results),
              ylim=c(0,1), las=2,
              ylab= "Test Accuracy", xlab="", col=rainbow(nrow(exp.benchmark.3$results)))
title(xlab = "Classifier", outer = TRUE, line = 4)
vals <- signif(exp.benchmark.3$results$avg.test.acc, 3)
text(mp, vals, labels = vals, pos = 3)
par(oldpar)
dev.off()

# prep.out("figures/results-benchmark-compare.pdf", width=std.width, height=std.height)
# oldpar <- par(xpd=NA, oma=c(2,0,0,0))
# mp <- barplot(rbind(xtabs(avg.test.acc ~ name, exp.benchmark$results),
#                     xtabs(avg.test.acc ~ name, exp.benchmark.2$results),
#                     xtabs(avg.test.acc ~ name, exp.benchmark.3$results)),
#               beside=TRUE,
#               ylim=c(0,1), las=2,
#               ylab= "Test Accuracy", xlab="", #col=rainbow(nrow(exp.benchmark.3$results)))
#               col=rep(heat.colors(3), nrow(exp.benchmark$results)))
# title(xlab = "Classifier", outer = TRUE, line = 0)
# vals <- signif(c(exp.benchmark$results$avg.test.acc,
#                  exp.benchmark.2$results$avg.test.acc,
#                  exp.benchmark.3$results$avg.test.acc), 3)
# text(mp, vals, labels = vals, pos = 3)
# par(oldpar)
# dev.off()

# Print in latex format tables for all the above experiments
xtable.custom(exp.benchmark$results, colnames=c("Classifier", "Average Training Accuracy",
                                                "Average Test Accuracy"),
              include.rownames=FALSE, digits=3)
