# Description: Compute diversity tables

switch(Sys.info()[['sysname']],
       # On Windows, this is Yue's directory
       Windows = {  work.dir <- "C:/Users/Yue/Desktop/Research_Project/research/stream-mining" },
       # On Linux, this is Phil's directory
       Linux = { work.dir <- "~/h/proj/ieee-stream-mining/code"  })
setwd(work.dir)

source("dodgers.r")

setup.test.train(d.byday.1hour, 70, 81)

run.sliding.windows.ens.nnet(5)
diff.table.s5 <- err.table

run.sliding.windows.ens.nnet(5, pass.previous.weights = FALSE)
diff.table.s5.dnpw <- err.table

run.sliding.windows.ens.nnet(15)
diff.table.s15 <- err.table

run.sliding.windows.ens.nnet(15, pass.previous.weights = FALSE)
diff.table.s15.dnpw <- err.table

run.sliding.windows.ens.nnet(30)
diff.table.s30 <- err.table

run.sliding.windows.ens.nnet(30, pass.previous.weights = FALSE)
diff.table.s30.dnpw <- err.table

run.sliding.windows.ens.nnet(50)
diff.table.s50 <- err.table

run.sliding.windows.ens.nnet(50, pass.previous.weights = FALSE)
diff.table.s50.dnpw <- err.table

#######################################
# Compute sum of columns of diff tables

mean.acc <- list()
mean.acc.dnpw <- list()

range.size <- seq(10, 50, 5)
for (size in range.size) {
  run.sliding.windows.ens.nnet(size)
  diff <- err.table
  run.sliding.windows.ens.nnet(size, pass.previous.weights = FALSE)
  diff.dnpw <- err.table
  
  mean.acc[size] <- mean(apply(diff.dnpw, 2, sum) / dim(diff.dnpw)[1])
  mean.acc.dnpw[size] <- mean(apply(diff, 2, sum) / dim(diff)[1])
}

plot(range.size, unlist(mean.acc), col=gray(0.25), type="o", pch=17,
     xlab="Sliding window size", ylab="Test accuracy",
     main="Mean accuracy of individual neural\nnetwork in the ensemble")
lines(range.size, unlist(mean.acc.dnpw), col=gray(0.1), type="o", pch=19)
legend("topright", c("Pass weights", "Do not pass weights"),
       pch=c(17,19), col=c(gray(0.25), gray(0.1)))

# (p <- ggplot(err.table, aes(variable, Name)) +
#    geom_tile(aes(fill = rescale),  colour = "white") +
#    scale_fill_gradient(low = "white", high = "steelblue"))


print.binary.heatmap <- function(diff.table)
{
  nRow = nrow(diff.table)
  stepRow <- max(1, floor(nRow/25)+1)
  labRow <- rep(NA, nRow)
  labRow[seq(1, nRow, stepRow)] <- seq(1, nRow, stepRow)
  nCol = ncol(diff.table)
  stepCol <- max(1, floor(nCol/25)+1)
  labCol <- rep(NA, nCol)
  labCol[seq(1, nCol, stepCol)] <- seq(1, nCol, stepCol)
  oldpar <- par(mar=c(0,0,0,0))
  heatmap(diff.table, scale = "none",
          Rowv = NA, Colv = NA,
          col = c(gray(1), gray(0.2)),
          revC = TRUE,
          xlab = "Model index (within ensemble)",
          ylab = "Row index (within sliding window)",
          labRow = labRow,
          labCol = labCol,
          main = "",
          margins = c(8,5))
  par(oldpar)
}

print.heatmap <- function(diff.table)
{
  nRow = nrow(diff.table)
  stepRow <- max(1, floor(nRow/25)+1)
  labRow <- rep(NA, nRow)
  labRow[seq(1, nRow, stepRow)] <- seq(1, nRow, stepRow)
  nCol = ncol(diff.table)
  stepCol <- max(1, floor(nCol/25)+1)
  labCol <- rep(NA, nCol)
  labCol[seq(1, nCol, stepCol)] <- seq(1, nCol, stepCol)
  oldpar <- par(mar=c(0,0,0,0))
  oldpar <- par(mar=c(0,0,0,0))
  heatmap(diff.table, scale = "none",
          Rowv = NA, Colv = NA,
          col = gray(seq(1,0,len=255)),
          revC = TRUE,
          xlab = "Model index (within ensemble)",
          ylab = "Row index (within sliding window)",
          labRow = labRow,
          labCol = labCol,
          cexRow = 1.3, cexCol = 1.3,
          main = "") 
  par(oldpar)
}

print.binary.heatmap(diff.table.s50)
print.binary.heatmap(diff.table.s50.dnpw)

prep.out("figures/gram-matrix-L50-passweights.pdf", width=6, height=6)
print.heatmap(compute.gram.matrix(diff.table.s50))
dev.off();
prep.out("figures/gram-matrix-L50-donotpassweights.pdf", width=6, height=6)
print.heatmap(compute.gram.matrix(diff.table.s50.dnpw))
dev.off();

print.binary.heatmap(diff.table.s15)
print.binary.heatmap(diff.table.s15.dnpw)
print.heatmap(compute.gram.matrix(diff.table.s15))
print.heatmap(compute.gram.matrix(diff.table.s15.dnpw))

print.heatmap(compute.gram.matrix(diff.table.s30))
print.heatmap(compute.gram.matrix(diff.table.s30.dnpw))

print.binary.heatmap(diff.table.s5)
print.binary.heatmap(diff.table.s15)
print.binary.heatmap(diff.table.s30)
print.binary.heatmap(diff.table.s50)

print.heatmap(compute.gram.matrix(diff.table.s5))
print.heatmap(compute.gram.matrix(diff.table.s15))
print.heatmap(compute.gram.matrix(diff.table.s50))

print.binary.heatmap(diff.table.s5.dnpw)
print.binary.heatmap(diff.table.s15.dnpw)
print.binary.heatmap(diff.table.s30.dnpw)
print.binary.heatmap(diff.table.s50.dnpw)

print.heatmap(compute.gram.matrix(diff.table.s5.dnpw))
print.heatmap(compute.gram.matrix(diff.table.s15.dnpw))
print.heatmap(compute.gram.matrix(diff.table.s50.dnpw))