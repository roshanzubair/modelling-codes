if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2010-01-01")
end <- as.Date("2018-04-13")

getSymbols("AMZN", src = "yahoo", from = start, to = end)
AMZN<-data.frame(as.matrix(AMZN))

library(data.table)
AMZN<-setDT(AMZN, keep.rownames = TRUE)[]
AMZN$Date<-as.Date(AMZN$rn)
AMZN$rn<-NULL

library(Hmisc)
AMZN$lag_year1 <- Lag(AMZN$AMZN.Close, 252)
months<-format(AMZN$Date,format="%B")

class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

AMZN<-cbind(AMZN,class.ind(months))
AMZN<-cbind(AMZN,class.ind(weekdays(AMZN$Date)))

#