# FUNCTION: sfcompgraph(m1, m2, sf1 = "sf", sf2 = "sf")
# PURPOSE: overlays the seasonal factors of the seasonal objects m1 and m2.
# By default the purely seasonal factors (d10 or s10) will be plotted;
# to instead plot the combined seasonal factors (d16 or s16) of m1 and
# /or m2, set sf1 = "cf" and/or sf2 = "cf"
# Sliding span diagnostics compare the adjustments from overlapping subspans of the series.
# we prefer the more stable adjustments.

# If the percent change between the maximum and the minimum
# (maximum percent difference, MPD) is greater than a cutoff (default 3%),
# MPD = (max(SFt)-min(SFt))/min(SFt) > 0.03, we flag the adjustment at time point t as unstable

# Month-to-mont (percent) changes:
# Multiplicative (%): MMt = (SAt - SAt-1)/SAt-1
# Additive: MMt = SAt - SAt-1
# MPD = max(MMt) - min(MMt) > 0.03

# Default cutoffs are:
# - 3% for seasonal factors and seasonally adjusted series.
# - 3% for month-to-month (quarter-to-quarter) and year-to-year change.
# - 2% for trading day factors (if compared).
# 5% instead of 3% is sued for highly variable irregular series.

# 85th percentile for seasonally adjusted series and
# 60th percentile for month-to-month (quarter-to-quarter) percent changes.

# There is a hinge value associated with the default cutoff value
# for the MPD 

# Only for multiplicative adjustments we can compare the seasonally adjusted values.
# Generally, an adjustment is considered acceptably stable when the percentage of
# unstable seasonal factors is less than 15% and
# the percentage of unstable month-to-month percent changes in the seasonal adjustment
# is less than 40%.
sfcompgraph <- function(m1, m2, sf1 = "sf", sf2 = "sf") {
  pd <- udg(m1, "freq") # Period, number of observations per year
  pd2 <- udg(m2, "freq")
  if (pd != 4 && pd != 12) {
    stop("monthly or quarterly only")
  }
  if (pd != pd2) {
    stop("series periods do not match")
  }
  
  require(seasonal)
  
  if (sf1 != "cf") {
    if (udg(m1, "samode") == "SEATS seasonal adjustment") {
      d10 <- series(m1, "s10") # SEATS Seasonal factors, seats.seasonal
    } else {
      d10 <- series(m1, "d10") # X-11 Seasonal factors, x11.seasonal
    }
  } else {
    if (udg(m1, "samode") == "SEATS seasonal adjustment") {
      d10 <- series(m1, "s16") # SEATS Combined adjustment factors, seats.adjustfac	
    } else {
      d10 <- series(m1, "d16") # X-11 Combined adjustment factors, x11.adjustfac	
    }
  }
  
  if (sf2 != "cf") {
    if (udg(m2, "samode") == "SEATS seasonal adjustment") {
      d10a <- series(m2, "s10") # SEATS seasonal factors, seats.seasonal
    } else {
      d10a <- series(m2, "d10") # X-11 seasonal factors, x11.seasonal
    }
  } else {
    if (udg(m2, "samode") == "SEATS seasonal adjustment") {
      d10a <- series(m2, "s16") # SEATS combined adjustment factors, seats.adjustfac	
    } else {
      d10a <- series(m2, "d16") # X-11 combined adjustment factors, x11.adjustfac	
    }
  }
  
  
  if (sf1 == sf2 && sf1 == "cf") {
    comb <- "Combined"
  } else if (sf1 == "cf" || sf1 == "cf") {
    comb <- "[Combined]"
  } else {
    comb <- ""
  }
  
  start.year <- min(start(d10)[1], start(d10a)[1])
  end.year <- max(end(d10)[1], end(d10a)[1])
  maxlen <- end.year - start.year + 1
  
  # print(start.year)
  # print(end.year)
  # print(maxlen)
  
  msf  <- d10
  msfa <- d10a
  for (v in 1:pd) {
    mm <- mean(subset(d10, cycle(d10) == v))
    msf[cycle(d10) == v] <- mm
    
    mm <- mean(subset(d10a, cycle(d10a) == v))
    msfa[cycle(d10a) == v] <- mm
  }
  
  ylimlp <- range(d10, d10a)
  upp <- maxlen * pd + pd
  midpt <- (maxlen + 1) / 2
  
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
  
  title(main = paste(comb, "Seasonal Factors -- ", deparse(substitute(m1)), "and", deparse(substitute(m2))))
  
  
  for (v in 1:pd) {
    start <- (v - 1) * (maxlen + 1) + 1 - start.year + start(d10)[1]
    
    end <- start + length(subset(d10, cycle(d10) == v)) - 1
    
    lines(start:end, subset(d10, cycle(d10) == v), col = "blue", lwd = 1)
    lines(start:end, subset(msf, cycle(d10) == v), col = "darkblue", lwd = 1)
    start <- (v - 1) * (maxlen + 1) + 1 - start.year + start(d10a)[1]
    
    end <- start + length(subset(d10a, cycle(d10a) == v)) - 1
    
    lines(start:end, subset(d10a, cycle(d10a) == v), col = "red", lwd = 1)
    lines(start:end, subset(msfa, cycle(d10a) == v), col = "darkred", lwd = 1)
  }
}
# END