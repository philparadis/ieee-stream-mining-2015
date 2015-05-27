source("senn.r")

library(foreign)

### Load Airlines Delay dataset

df.airlines <- read.arff("data/airlines.arff")

### Run SENN

obj.name <- "objects/senn-airlines"
if (file.exists(obj.name)) {
  exp.nnet.ensemble <- readRDS(obj.name)
} else {
  model <- senn(.~Delay, data=df.airlines,
                
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
  saveRDS(exp.nnet.ensemble, file=obj.name)
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
