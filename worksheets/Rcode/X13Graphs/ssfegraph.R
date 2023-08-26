# FUNCTION: ssfegraph(m1, m2)
# PURPOSE: Sum of squared forecast error difference graph
# ARGUMENTS: m1 and m2 are objects of class "seas"
# returned from a call to seas() with the option history.estimates = "fcst"
ssfegraph <- function(m1, m2) {
  require(seasonal)
  fq1 <- udg(m1, "freq") # Period, number of observations per year
  fq2 <- udg(m2, "freq")
  if (fq1 != fq2) stop("Series must have the same period.")
  
  h1 <- series(m1, "fce") # history.fcsterrors
  h2 <- series(m2, "fce")
  
  if (time(h1)[1] != time(h2)[1]) {
    if (time(h1)[1] > time(h2)[1]) {
      first.time <- start(h1)
    } else {
      first.time <- start(h2)
    }
    
    ftp <- first.time
    ftp[2] <- ftp[2] - 1
    if (ftp[2] == 0) {
      ftp[2] <- 12
      ftp[1] <- ftp[1] - 1
    }
    first.time.s <- first.time
    first.time.s[2] <- first.time.s[2] + (fq1 - 1)
    if (first.time.s[2] > fq1) {
      first.time.s[1] <- first.time.s[1] + 1
      first.time.s[2] <- first.time.s[2] - fq1
    }
    ftps <- first.time.s
    ftps[2] <- ftps[2] - 1
    if (ftps[2] == 0) {
      ftps[2] <- fq1
      ftps[1] <- ftps[1] - 1
    }
    
    z1 <- window(h1, start = first.time)
    z2 <- window(h2, start = first.time)
    if (time(h1)[1] > time(h2)[1]) {
      z2[, 1] <- z2[, 1] - window(h2, start = ftp, end = ftp)[1]
      z2[, 2] <- z2[, 2] - window(h2, start = ftps, end = ftps)[2]
      z2[1:11, 2] <- 0
    } else {
      z1[, 1] <- z1[, 1] - window(h1, start = ftp, end = ftp)[1]
      z1[, 2] <- z1[, 2] - window(h1, start = ftps, end = ftps)[2]
      z1[1:11, 2] <- 0
    }
    
    
    
    h1 <- z1
    h2 <- z2
  }
  
  
  
  diff1 <- h1[, 1] - h2[, 1]
  diff12 <- h1[, 2] - h2[, 2]
  
  sdiff1 <- (diff1 * length(diff1)) / h2[length(h2[, 1]), 1]
  sdiff12 <- (diff12 * (length(diff12) - (fq1 - 1))) / h2[length(h2[, 2]), 2]
  
  
  ttl <- paste("Squared Forecast Error Differences,", deparse(substitute(m1)), "-", deparse(substitute(m2)))
  plot(sdiff1,
       ylim = range(sdiff1, sdiff12), main = ttl,
       ylab = "", xlab = ""
  )
  lines(sdiff12, col = "red")
  abline(h = 0,  col = "gray")
}
# END