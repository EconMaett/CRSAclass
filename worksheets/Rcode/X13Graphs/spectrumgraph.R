# spectrumgraph.R ----

# FUNCTION: spectrumgraph(seasonal object, component)
# PURPOSE:  plots the spectrum graph calculated by X-13ARIAM-SEATS
# OPTIONS: 
# - component = "rsd" for residuals
# - component = "ori" for original
# - component =  "sa" for seasonally adjusted series
# - component = "irr" for irregular
# Note that the spectrum can only be calculated for series
# of monthly frequency.
# X-13ARIMA-SEATS only produces spectral diagnostics for monthly, but not for quarterly seris.
# Time doman: Sequence of values with regard to time interval.
# Frequency domain: Values represented as a combination of sine and cosine waves
# xt = at*sin(w) + bt*cos(w)
# Periodogramm os spectrum plots the squared amplitudes versus the frequencies.
# Seasonal frequencies:
# Seasonal frequencies are marked at S1, S2, S3, S4, S5
# Something happens every 12 months -> 1/12 cycles per month
# Something happens every 6 months -> 1/6 (2/12) cycles per month
# 3 months (every quarter) -> 1/3 (4/12) cycles per month
# Seasonal frequencies marked at 1/12, 2/12, 3/12, 4/12, 5/12
# Prominent spectral peaks at 1/12, 2/12, 3/12 or 4/12 indicate the presence of seasonal effects.
# In practice, discount peaks at 5/12 and 6/12.
# Trading day frequencies:
# To see daily effect with a period of seven days:
# - 365.25/12 = 30.4375
# - 30.4375/7 = 4.348
# Trading day peaks are marked at T1 (0.348) and T2 (0.432)
# A prominent spectral peak at 0.348 indicates that trading day effects are present.
# Visually significant peaks are greater than the median and taller than
# 6/52 of the spectrum range (max-min).
# One star = 1/52 of the range, drawn with asterisks.
# Always try to eliminate peaks at 1/12, 2/12, 3/12, 4/12, or TD 0.348
# from the spectrum of the seasonally adjusted series and the residuals.

# Sig Ori Peaks: 
# Indicates the visually significant seasonal and trading day peaks in the spectrum
# of the (possibly differenced, transformed, prior-adjusted) original series.
# If the peak hieght is 6.0 or greater than the udg entry ends with a "+"
# to indicate the peak is greater than the median, it is visually significant.
# - spcrsd.s1, .s2, .s3, .s4, .s5
# - spcrsd.t1, t2

# Tukey Ori Peaks
# Lists the seasonal and trading day frequencies where the Tukey
# spectrum of the original series has a peak with a significance level
# greater than 0.99 or 0.90
# - spcori.tukey.s1, .s2, .s3, .s4, .s5
# - spcori.tukey.td
spectrumgraph <- function(m, comp) {
  require(seasonal)
  if (udg(m, "freq") != 12) {
    stop("monthly only")
  }
  if (comp == "rsd") {
    spcmp <- series(m, "spr")
    ttl   <- "Residuals"
  } else if (comp == "sa") {
    if (udg(m, "samode") == "SEATS seasonal adjustment") {
      spcmp <- series(m, "s1s") # spectrum.specseatssa
    } else {
      spcmp <- series(m, "sp1") # spectrum.specsa
    }
    ttl <- "Seasonally Adjusted Series"
  } else if (comp == "ori") {
    spcmp <- series(m, "sp0") # spectrum.specorig
    ttl <- "Original Series"
  } else if (comp == "irr") {
    if (udg(m, "samode") == "SEATS seasonal adjustment") {
      spcmp <- series(m, "s2s") # spectrum.specseatsirr
    } else {
      spcmp <- series(m, "sp2") # spectrum.specirr
    }
    ttl <- "Irregular"
  } else {
    stop("Component must be ori, rsd, sa, or irr")
  }
  
  mincmp <- min(spcmp[, 2])
  maxcmp <- max(spcmp[, 2])
  medcmp <- median(spcmp[, 2])
  
  vs <- (maxcmp - mincmp) * 6 / 52
  
  fq <- list()
  de <- list()
  
  fq[seq(1, 181, 3)] <- spcmp[, 1]
  fq[seq(2, 182, 3)] <- spcmp[, 1]
  fq[seq(3, 183, 3)] <- spcmp[, 1]
  de[seq(1, 181, 3)] <- mincmp
  de[seq(2, 182, 3)] <- spcmp[, 2]
  de[seq(3, 183, 3)] <- mincmp
  
  plot(0, xlim = range(seq(0, 0.5, 1 / 12)), type = "n", ylim = range(spcmp[, 2], maxcmp + vs / 6), ann = FALSE, )
  title(main = paste("Spectrum of the", ttl, " -- ", deparse(substitute(m))))
  title(xlab = "Frequency")
  
  abline(v = seq(0, 1 / 2, 1 / 12), lty = 5, col = "red")
  abline(v = c(0.3482, 0.432), lty = 5, col = "orange")
  lines(fq, de, lty = 1, col = "black", lwd = 2)
  segments(x0 = 0.51, x1 = 0.51, y0 = medcmp, y1 = medcmp + vs, col = "blue", lwd = 2)
  segments(y0 = medcmp, y1 = medcmp, x0 = 0.505, x1 = 0.515, col = "blue", lwd = 2)
  for (q in 1:5) {
    kc <- paste0("spc", comp, ".s", q)
    tp <- udg(m, kc)
    
    if (tp[1] != "nopeak" && length(tp) == 2) {
      if (as.numeric(tp[1]) >= 6.0 && tp[2] == "+") {
        text(x = q / 12, y = spcmp[1 + q * 10, 2] + vs / 6, labels = "S", col = "blue")
      }
    }
  }
  tdf <- c(43, 53)
  for (q in 1:2) {
    kc <- paste0("spc", comp, ".t", q)
    tp <- udg(m, kc)
    
    if (tp[1] != "nopeak" && length(tp) == 2) {
      if (as.numeric(tp[1]) >= 6.0 && tp[2] == "+") {
        text(x = spcmp[tdf[q], 1], y = spcmp[tdf[q], 2] + vs / 6, labels = "T", col = "blue")
      }
    }
  }
}
# END