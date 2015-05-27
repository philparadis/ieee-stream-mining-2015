######################################
switch(Sys.info()[['sysname']],
       # On Windows, this is Yue's directory
       Windows = {  work.dir <- "C:/Users/Yue/Desktop/Research_Project/research/stream-mining" },
       # On Linux, this is Phil's directory
       Linux = { work.dir <- "~/h/proj/ieee-stream-mining/code"  })
setwd(work.dir)
source("dodgers.r")

# Set parameters
margins <-c(4, 4, 0.8, 0.5)
std.width <- 7
std.height <- 3

std.maxit <- 50
std.h.size <- 4

# Set color palette
oldpalette <- palette()
palette(gray(seq(0.1, 0.4, len=8)))

# Create 'figures' and 'objects' subdirectories if they don't exist
setwd(work.dir)
dir.create("figures", showWarnings = FALSE)
dir.create("objects", showWarnings = FALSE)

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
   plot(test.accuracy ~ windows.size, col=2, type="o", pch=17,
        ylim=range(c(0.5,1)),
        ylab="Accuracy", xlab="Sliding windows size")
   grid(col="lightblue4")
   legend("bottomright", c("Test Accuracy"), col=c(2), pch=c(17), bg="white")
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

prep.out("figures/results-nnet-ens-maxit.pdf", width=std.width, height=std.height+1.5)
par(mar=margins)
plot(exp.nnet.ens.maxit.pw$results$train.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
     col=4, type="o", pch=17,
     ylim=range(c(exp.nnet.ens.maxit.pw$results$test.accuracy,
                  exp.nnet.ens.maxit.dnpw$results$train.accuracy,
                  exp.nnet.ens.maxit.dnpw$results$test.accuracy,
                  0.5, 1.0)),
     ylab="Accuracy", xlab="Maximum number of iterations (maxit)")
lines(exp.nnet.ens.maxit.pw$results$test.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=5, pch=19)
lines(exp.nnet.ens.maxit.dnpw$results$train.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=8, pch=0)
lines(exp.nnet.ens.maxit.dnpw$results$test.accuracy ~ exp.nnet.ens.maxit.pw$results$maxit,
      type="o", col=9, pch=8)
grid(col="lightblue4")
legend("bottomright", c("Pass weights (training set)", "Pass weights (testing set)",
                        "Do not pass weights (training set)", "Do not pass weights (testing set)"),
       col=c(4,5,8,9), pch=c(17,19,0,8), bg="white")
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
   plot(test.accuracy ~ windows.size, col=2, type="o", pch=17,
        ylim=range(c(0.5,1)),
        ylab="Accuracy", xlab="Sliding windows size")
   grid(col="lightblue4")
   legend("bottomright", c("Test Accuracy"), col=c(2), pch=17, bg="white")
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
plot(exp.nnet.minibatch$results$test.accuracy ~ exp.nnet.minibatch$results$windows.size,
     col=3, type="o", pch=17,
     ylim=range(exp.nnet.minibatch$results$test.accuracy,
                exp.nnet.ensemble$results$test.accuracy,
                exp.nnet.ensemble.dnpw$results$test.accuracy,
                0.5, 1.0),
     ylab="Accuracy", xlab="Sliding windows size")
lines(exp.nnet.ensemble$results$test.accuracy ~ exp.nnet.ensemble$results$windows.size,
      col=4, type="o", pch=19,)
lines(exp.nnet.ensemble.dnpw$results$test.accuracy ~ exp.nnet.ensemble.dnpw$results$windows.size,
      col=2, type="o", pch=15,)
grid(col="lightblue4")
legend("bottomright",
       c("Sliding Mini-batch Neural Networks",
         "Sliding Ensemble Neural Networks",
         "Sliding Ensemble Neural Networks (do not pass weights)"),
       col=c(3,6,4,2), pch=c(17,19,15), bg="white")
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
   plot(1000*time/(70-windows.size+1) ~ windows.size, col=4, type="o", pch=17,
        ylim=range(1000*c(time/(70-windows.size+1),
                          exp.nnet.ens.time.donotpass$results$time/(70-exp.nnet.ens.time.donotpass$results$windows.size+1))),
        #ylim=range(exp.nnet.ens.time.pass$results$time, exp.nnet.ens.time.donotpass$results$time),
        ylab="Time / window (milliseconds)", xlab="Sliding windows size")
})
with(exp.nnet.ens.time.donotpass$results, {
   lines(1000*time/(70-windows.size+1) ~ windows.size, type="o", col=5, pch=19)
   grid(col="lightblue4")
   legend("bottomright",
          c("Pass weights to next window", "Do not pass weights"),
          col=c(4,5), pch=c(17,19), bg="white")
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

prep.out("figures/results-benchmark.pdf", width=std.width, height=3.2)
oldpar <- par(xpd=NA, oma=c(6,0,0,0), mar=c(1.1, 4.1, 1, 0.5))
mp <- barplot(xtabs(avg.test.acc ~ name, exp.benchmark$results),
        ylim=c(0,1), las=2,
        ylab= "Test Accuracy", xlab="",
        col=gray(0.75))
        #col=rainbow(nrow(exp.benchmark$results)))
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
              ylab= "Test Accuracy", xlab="",
              col=rainbow(nrow(exp.benchmark.2$results)))
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
              ylab= "Test Accuracy", xlab="",
              col=gray(0.75))
              #col=rainbow(nrow(exp.benchmark.3$results)))
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
