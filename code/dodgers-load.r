
# EDIT YOUR SETTINGS FOR LINUX HERE
######################################
switch(Sys.info()[['sysname']],
       # On Windows, this is Yue's directory
       Windows = {  work.dir <- "C:/Users/Yue/Desktop/Research_Project/research/stream-mining" },
       # On Linux, this is Phil's directory
       Linux = { work.dir <- "~/h/proj/ieee-stream-mining-code"  })
setwd(work.dir)

library(xtable)

# Global variable definitions
save.to.pdf <- TRUE

# TODO: Review how NA values are handled...
# Make sure we're handling them properly...

# TODO: Mention that clearly, our dataset is small enough
# such that we don't need stream mining technique to handle it.
# However, we pretend that our resources are limited such that we
# can't fit all the data in memory at once and as such we force ourselves
# to run through the data in one-pass via sliding windows

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

