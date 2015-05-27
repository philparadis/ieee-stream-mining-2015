# Load config.r, as usual
switch(Sys.info()[['sysname']],
       # On Windows, this is Yue's directory
       Windows = {  work.dir <- "C:/Users/Yue/Desktop/Research_Project/research/stream-mining" },
       # On Linux, this is Phil's directory
       Linux = { work.dir <- "~/h/proj/ieee-stream-mining/code"  })
setwd(work.dir)
source("dodgers-load.r")

# Create 'figures' subdirectory if it doesn't exist
setwd(work.dir)
dir.create("figures", showWarnings = FALSE)

# Load datasets, do pre-processing
source("dodgers-load.r")

################################
# Start plotting graphs
################################

# Plot a representative week

indices.1 <- (288*22+1):(288*26)
indices.2 <- (288*26+1):(288*30)
# 
# prep.out("figures/typical-week.pdf", width=7, height=3)
# oldpar <- par(mfcol=c(2,1), mar=c(2,0,1,1), oma=c(3,4,0,0))
# indices <- indices.1
# plot(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
#      type="l", xaxs="i", lwd=0, col="WHITE",
#      xlab="", ylab="")
# plot.all.events(e.dodgers, lwd=1, lty=1,
#                 col.begin=rgb(0,100,255,255,maxColorValue=255),
#                 col.end=rgb(255,0,00,255,maxColorValue=255))
# lines(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
#      type="l", xaxs="i", lwd=1)
# indices <- indices.2
# plot(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
#      type="l", xaxs="i", lwd=0, col="WHITE",
#      xlab="", ylab="")
# plot.all.events(e.dodgers, lwd=1, lty=1,
#                 col.begin=rgb(0,100,255,255,maxColorValue=255),
#                 col.end=rgb(255,0,00,255,maxColorValue=255))
# lines(d.dodgers[indices,"datetime"], d.dodgers[indices,"count"],
#       type="l", xaxs="i", lwd=1)
# title(xlab = "Day of the week",
#       outer = TRUE, line = 1)
# title(ylab = "Car counts per 5 minutes interval",
#       outer = TRUE, line = 2.5)
# par(oldpar)
# dev.off()


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
