# FUNCTION: sigraph(seasonal object)
# PURPOSE: for a series with an X11 adjustment, 
# plots the seasonal factors, and the unadjusted and adjusted SI ratios
# SI Ratio plot:
# Seasonal factors (solid line, Table D10) plotted against the 
# Unadjusted SI ratios (green stars, Table D 8) and the 
# Replacement Values (red dots, Table D 9)
sigraph <- function(m) {
  pd <- udg(m, "freq")
  if (pd != 4 && pd != 12) {
    stop("monthly or quarterly only")
  }
  require(seasonal)
  d8  <- series(m, "d8")  # Un-adjusted SI ratios D8 = B1 / D7 or D8 = B1 - D7
  d9  <- series(m, "d9")  # Final replacement values for SI ratios at extreme values D9 = D1 / D7 or D9 = D1 - D7
  d10 <- series(m, "d10") # Seasonal factors
  
  maxlen <- length(subset(d10, cycle(d10) == 1))
  for (v in 2:pd) {
    maxlen <- max(maxlen, length(subset(d10, cycle(d10) == v)))
  }
  
  # sirat <-d8
  # sirat[!is.na(d9)] <- d9[!is.na(d9)]
  
  # rsi <- d8
  # rsi[is.na(d9)] <- NA
  
  msf <- d10
  for (v in 1:pd) {
    mm <- mean(subset(d10, cycle(d10) == v))
    msf[cycle(d10) == v] <- mm
  }
  
  ylimlp <- range(d10, d8, d9[!is.na(d9)])
  upp    <- maxlen * pd + pd
  midpt  <- (maxlen + 1) / 2
  
  plot(1:upp, type = "n", xaxt = "n", ylim = ylimlp, bty = "o", las = "1", ann = FALSE, xaxs = "i")
  
  abline(v = seq(maxlen + 1, upp, maxlen + 1), lty = 1, col = "gray")
  
  if (pd == 12) {
    title(xlab = "Month")
    for (v in 1:12) {
      mtext(month.abb[v], side = 1, at = midpt * (2 * v - 1))
    }
  } else {
    title(xlab = "Quarter")
    for (v in 1:4) {
      mtext(paste("Q", v), side = 1, at = midpt * (2 * v - 1))
    }
  }
  
  title(main = paste("SI Ratios -- ", deparse(substitute(m))))
  
  
  for (v in 1:pd) {
    start <- (v - 1) * (maxlen + 1) + 1
    end <- start + length(subset(d10, cycle(d10) == v)) - 1
    lines(start:end, subset(d10, cycle(d10) == v), col = "blue", lwd = 1)
    lines(start:end, subset(msf, cycle(d10) == v), col = "darkblue", lwd = 1)
    points(start:end, subset(d8, cycle(d10) == v), col = "gray", pch = 21)
    points(start:end, subset(d9, cycle(d10) == v), col = "red", pch = 19)
  }
}
# END