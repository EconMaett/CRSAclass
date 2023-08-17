# Exercise 6 - Seasonal Adjustment ----

## Prep ----
if(!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

# NEED: source X13Graphs.r for spectrumgraph(m, comp)
# source(file = "worksheets/worksheet-answers/Rcode/X13Graphs.r")
source(file = "worksheets/worksheet-answers/Rcode/X13Graphs/sfcompgraph.R")
source(file = "worksheets/worksheet-answers/Rcode/X13Graphs/sigraph.R")
source(file = "worksheets/worksheet-answers/Rcode/X13Graphs/spectrumgraph.R")
source(file = "worksheets/worksheet-answers/Rcode/X13Graphs/ssfegraph.R")
source(file = "worksheets/worksheet-answers/Rcode/X13Graphs/yearyeargraph.R")

## Part 1 ----
# Import the following monthly time series running from 
# 1994 01 to 2019 08
f   <- "data/cr/CR Regional IPC.dat"
ipc <- import.ts(file = f, format = "datevalue")

# plot the series
plot(ipc)
graphics.off()

### Question 1 ----
# Examine the seasonality of the series. 
# Should it be seasonally adjusted?

# First we create a seasonal object (object of class "seas") with
# just the default options activated.
# Note that we do not do any seasonal adjustment here:
c <- seas(ipc, 
          x11 = "",
          outlier.types = "all",
          automdl = "",
          slidingspans = "",
          regression.aictest = NULL,
          )

summary(c)
# The regARIMA model that was automatically chosen:
# Transform: Log. 
# ARIMA: (1 2 1)(0 1 1).
# Outliers: TC1998.Nov, LS2003.Nov, LS2004.Nov, TC2004.Feb, AO2004.Dec, LS2007.Nov

# We can plot the final series and see that no seasonal adjustment has taken place.
ts.plot(original(c), final(c), col = "black", "red")
graphics.off()

# We use the spectrumgraph(m, comp) function to plot the spectrum graph calculated by X-13ARIAM-SEATS
# The options for the components are original, seasonally adjusted, irregular, and residual.

spectrumgraph(m = c, comp = "ori")
# There are no seasonal peaks in the spectrum of the original series
# - not even any non-visually significant peaks

spectrumgraph(m = c, comp = "sa")
spectrumgraph(m = c, comp = "irr")
spectrumgraph(m = c, comp = "rsd")
spec.ar(ipc)
qs(c)
out(c)
udg(c, "f3.m07") # 0.942
# From the HTML document "diag list.html":
# M7: The amount of moving seasonality present relative 
# to the amount of stable seasonality

monthplot(c)
abline(h = 1)
graphics.off()


### Question 2 ----
# We want to create seasonal regressors to be able to use an F-test for seasonality.
# We hard-code the ARIMA model found beofeore,
# and we include seasonal regressors with regression.variables = c("seasonal").
c_seasreg <- seas(ipc, 
                  x11 = "",
                  outlier.types = "all", 
                  automdl = "", 
                  slidingspans = "", 
                  regression.aictest = NULL, 
                  regression.variables = c("seasonal", "TC2017.Jan"),
                  arima.model = "(1 1 0)(1 0 0)"
                  )

summary(c_seasreg)
# We created regressors Jan-Nov, which alone are all insignificant.
# In that case, we do not even apply an F-test.
# This confirms that there is almost no seasonality present in the original data.

# WE can use monthplot to examine the seasonality.
# Note that the scale of the y-axis is *very* small.
monthplot(c)
graphics.off()


## Part 2 ----
# Analysis of fuel dealers data

# Create a seasonal object called "fd.x11" for  .\data\retail\fuel dealers.dat. 

# Use the ARIMA model (0 1 1)(0 1 1). Perform an X-11 adjustment.

# Import monthly data from fuel dealers from 1992 01 to 2015 06:
f <- "data/retail/Fuel dealers.dat"
x <- import.ts(file = f, format = "datevalue")


# There is clearly seasonality present!
plot(x)
graphics.off()

fd.x11 <- seas(x,
               x11 = "",
               arima.model = "(0 1 1)(0 1 1)",
               slidingspans = "",
               history = ""
               )

summary(fd.x11)
# Transform: log
# ARIMA(0 1 1)(0 1 1) -> Airline model
# AICc 3504
# Weekday
# Ao2006.Jan, Ao2007.Feb


### Question 1 ----
# Describe the spectrum of the original series. 
# Use spectrumgraph(fd.x11, “ori”).
spectrumgraph(m = fd.x11, comp = "ori")
graphics.off()

# Answer: There are visibly significant peaks at the seasonal lags indicated 
# in blue, at S1, S2, and S5.

### Question 2 ----
# Look at the spectrum of the seasonally adjusted series,
# the irregular, and the residuals.
# Use spectrumgraph(seasonal object, component).
# Is there evidence of residual seasonality or residual calendar effects?
spectrumgraph(m = fd.x11, comp = "sa")
spectrumgraph(m = fd.x11, comp = "irr")
spectrumgraph(m = fd.x11, comp = "rsd")
graphics.off()
# Answer: There are no seasonal peaks in the spectrum of the
# seasonally adjusted series or the irregular,
# so there is no residual seasonality.

# There are no trading day peaks in any spectra,
# so there are no residual calendar effects.

# There is one visually significant seasonal peak at S5
# in the spectrum of the residuals,
# and there is an almost significant seasonal peak at S1
# - 5.5 stars - which may indicate problems modeling the
# seasonal pattern.


### Question 2 ----

# The seasonal filter was selected based on the global moving
# seasonality ration (GMSR).
# What is the GMSR for this series?
# What seasonal filter was selected?

udg(fd.x11, "sfmsr") # "3x5"
# "3x5" was the automatically selected seasonal filter

udg(fd.x11, "finaltrendma") # 13 
# 13-MA was the automatically selected trend filter

udg(fd.x11, "f2.is") # 3.52
# The final irregular/seasonal Ratio from Table D10; 
# also called the global moving seasonality ratio.

udg(fd.x11, "f2.ic") # 1.23
# The final irregular/trend ratio from Table D12


# Re-run X-11 with the seasonal filter hard-coded using
# x11.seasonalma = "s3x5"
# sliding span analysis activated with default settings
# slidingspans = ""
# and the history spec activated
# history = ""
fd.x11.hardfilter <- seas(x,
                          x11 = "",
                          x11.seasonalma = "s3x5",
                          slidingspans = "",
                          history = ""
                          )

summary(fd.x11.hardfilter)
# Transform: log
# ARIMA: (3 1 1)(0 1 1)
# AICc: 3531



### Question 3 ----
# Look at the sliding spas diagnostics.
# Is this adjustment acceptably stable?
# Why or why not?

udg(fd.x11.hardfilter, "r02.lag00.aar.all")
names(udg(fd.x11.hardfilter))

udg(fd.x11.hardfilter, "s2.a.per")[3] # 28.07
# SF% - s2.a.per[3]:
# Percent of months (quarters) with a maximum
# absolute percent change of the seasonal factors greater
# than the threshold (of 3)


udg(fd.x11.hardfilter, "s2.d.per")[3] # 21.239
# MM% - s2.d.per[3]:
# Percent of months (quarters) with a maximum
# absolute difference of period-to-period change in the
# seasonally adjusted series greater than the threshold (of 3)


# Answer: 
# 26% of months have failing seasonal factors and 
# 22% of months have a failing month-to-month change.

# This is **not** acceptably stable, since we would like
# the percentage of failing seasonal factors to be 
# below 15% and at least below 25%.

# The month-to-month changes are acceptable, as we want
# these to be below 40%.


### Question 4 ----
# Look at the output file for the history tables.
# Which months have the largest revisions? 

out(fd.x11.hardfilter)
# Answer:
# The largest revisions are in March.


### Question 5 a ----
# Look at the graph of the seasonal factors and SI ratios by month,
# using the sigraph function, or table D9.
# Which months have the most replaced SI ratios?

# We use the sigraph(seasonal object) function, which for a 
# series with an X11 adjustment, 
# plots the seasonal factors, 
# and the unadjusted and adjusted SI ratios
sigraph(m = fd.x11.hardfilter)
graphics.off()

# Answer:
# February and March both have 8 months
# with a replaced SI ratio.

# January and December both have 7 replaced values.


### Question 5 b ----
# Create a new seasonal object called fd.x11.sl for the series with
# the sigma limits raised to (1.8, 2.8).

# From the X-13 user manual,
# Chapter 7.19 X11.

# sigmalim: lower and upper sigma limits used to downweight extreme irregular values
# in the internal seasonal adjustment iterations. 
# A missing value defaults to (1.5, 2.5) 
# sigmalim = (, 3.0) defaults to (1.5, 3.0)

fd.x11sl <- seas(x,
                 x11 = "",
                 arima.model = "(0 1 1)(0 1 1)",
                 x11.sigmalim = c(1.8, 2.8),
                 slidingspans = "",
                 history = ""
                 )

summary(fd.x11sl)
# Transform: log
# ARIMA: (0 1 1 )(0 1 1)
# AICc: 3504


# Are there fewer replaced SI ratios in the months identified in 5a?
sigraph(m = fd.x11sl)
graphics.off()

# Answer: 
# With the higher limits:
# - January has 4 replacements (down 3)
# - February has 7 (down 1)
# - March has 7 (down 1)
# - December has 6 (down 1)



# Create a new seasonal object called fd.seats"
# running a default "seats" adjustment instead of x-11.
# Keep the airline model (Transform = "log", ARIMA(0,1,1)(0,1,1))
# and include sliding spans and history (slidingspans = "", history = "")

fd.seats <- seas(x,
                 transform.function = "log",
                 arima.model = "(0 1 1)(0 1 1)",
                 slidingspans = "",
                 history = ""
                 )

summary(fd.seats)
# Transform: log
# ARIMA: (0 1 1)(0 1 1)
# Weekday, Ao2006.Jan, Ao2007.Feb
# AICc: 3504


# Create a new seasonal object called "fd.seats2",
# which performs a default "seats" adjustment and starts
# the model span in 2004.01.
# Keep the airline model, and include sliding spans and history.

# Note that for SEATS, there is only a series.span spec-argument pair,
# not a model.span.

fd.seats2 <- seas(x,
                  transform.function = "log",
                  arima.model = "(0 1 1)(0 1 1)",
                  slidingspans = "",
                  history = "",
                  series.span = "2004.1,"
                  )

summary(fd.seats2)
# Transform: log
# ARIMA: (0 1 1)(0 1 1 )
# AICc 1695
# Weekday, Ao2004.Mar, Ao2006.Jan, Ao2007.Feb


### Question 6 ----
# Compare the regARIMA models from fd.seats and fd.seats2.
# Which model has better model diagnostics?
udg(fd.seats, "aicc")  # 3530.564
udg(fd.seats2, "aicc") # 1694.668
# The AIcc is much lower for the second model but the comparison is
# unfair, because it excludes more dat.

udg(fd.seats, "modelspan")  # 1992-01 to 2015-06
udg(fd.seats2, "modelspan") # 2004-01 to 2015-06
# Model Span - modelspan:
# Span of data used to estimate regARIMA model coefficients.
# Since these are different, any comparison is somewhat unfair.


# Answer: 
# The full span model fd.seats has 
# 7 failing LBQ, including lag  24.
udg(fd.seats, "nlbq") # 7
udg(fd.seats2, "nlbq") # 0
# # LBQ Fail - nlbq
# Number of the lags from 1 to 24 with significant Ljung-Box Q statistic
# The full model has 7 significant LBQs.

udg(fd.seats, "lblags") # 6, 11, 13, 15, 16, 23, 24
udg(fd.seats2, "lblags") # 0
# Sig LBQ - lblags:
# List of lags with significant LBQ.
# The full model has 7 significant LBQs, including at seasonal 
# lags 6 and 24

# It has 4 lags with a significant ACF,
# but they are at lags 5, 11, and 23, which are unimportant,
udg(fd.seats, "sigacflags") # 5, 11, 22, 23
udg(fd.seats2, "sigacflags") # 0
# Sig ACF - sigacflags
# Lags with significant autocorrelation in the residuals


# and the full model has a visually significant peak at S5 in the residuals,
# which we do not worry about normally when the other
# seasonal frequencies are not peaks, but S1 is an almost 
# visually significant peak (5.5 stars).
udg(fd.seats, c("spcrsd.s1", # "4.8" "+"
                "spcrsd.s2", # "nopeak"
                "spcrsd.s3", # "nopeak"
                "spcrsd.s4", # "nopeak"
                "spcrsd.s5", # 2.4
                "spcrsd.t1", # "nopeak"
                "spcrsd.t2"  # "nopeak"
                ))
# Resid peaks - spcrs.s1-s4/.td1-td2
# Indicates the visually significant seasonal and trading day peaks
# in the spectrum of the model residuals

# If the peak height is 6.0 or greater and the udg entry ends
# with a "+" to indicate the peak is greater than the median, 
# it is visually significant.

# The model with the shorter span, fd.seats2,
# has no LBC or ACF failures and no seasonal or trading day peaks
# in the spectrum of the residuals.
udg(fd.seats2, "nlbq") # 0
udg(fd.seats2, c("spcrsd.s1", # "nopeak"
                 "spcrsd.s2", # "nopeak"
                 "spcrsd.s3", # "nopeak"
                 "spcrsd.s4", # "nopeak"
                 "spcrsd.s5", #  "nopeak"
                 "spcrsd.t1", # "nopeak"
                 "spcrsd.t2"  # "nopeak"
                 ))
# But forecasts are a little worse for the full model.
udg(fd.seats, "aape.0")  # 11.96629 
udg(fd.seats2, "aape.0") # 14.16904
# aa FcE (3-yr) - aape.0
# Average absolute percentage error of forecasts in the last three years.
# An average of the 1-step ahead to 12/4-step ahead forecasts of the data
# with one, two, and three years removed.
# A comparison of forecast errors is always approbriate.


### Question 7 ----
# Compare the sliding spans diagnostics and the revisions from the
# four adjustments fd.x11, fd.x11sl, fd.seats, and fd.seats2.
# Which adjustment has the greatest stability?

# Answer: 
# The sliding span results are better for fd.seats2 than for either
# of the X-11 adjustments.

# They look best for fd.seats, but the span length is 102 for the other
# three models and 198 for fd.seats, so this comparison is problematic.
udg(fd.x11, "ssa")[2]    # 102
udg(fd.x11sl, "ssa")[2]  # 102
udg(fd.seats, "ssa")[2]  # 198
udg(fd.seats2, "ssa")[2] # 102
# Span Length - ssa[2]:
# Length of each span

# We can compare the average SA and MMA revisions in
# fd.x11, fd.x11sl, and fd.seats because they
# have the same start date, 2000.01.

# fd.seats has the largest revision in the seasonal adjustment,
# 2.6 compared to 2.3 and 2.2 for the X-11 adjustments.
udg(fd.x11,   "r01.lag00.aar.all") # 2.338174 ~ 2.3
udg(fd.x11sl, "r01.lag00.aar.all") # 2.259691 ~ 2.3
udg(fd.seats, "r01.lag00.aar.all") # 2.634587 ~ 2.6
# SA.AAR - r01.lag00.aar.all:
# Average absolute percent revisions of the seasonal adjustments
udg(fd.x11, "r01.aarmode")
# But the smallest revision in the month-to-month change,
# 1.67, compared to 1.83 and 1.76.
# NOTE: 
# MM.AAR - r02.lag00.aar.all:
# Average absolute revision of the month-to-month percent change of the adjustments.
# is not accessible through the seasonal package anymore...

# We can set the history start date to 2009.01 for all four adjustments,
# to 2004.01?
fd.x11 <- seas(x,
               x11 = "",
               arima.model = "(0 1 1)(0 1 1)",
               slidingspans = "",
               history = "",
               series.span = "2004.1,"
               )

fd.x11sl <- seas(x,
                 x11 = "",
                 arima.model = "(0 1 1)(0 1 1)",
                 x11.sigmalim = c(1.8, 2.8),
                 slidingspans = "",
                 history = "",
                 series.span = "2004.1,"
                 )

fd.seats <- seas(x,
                 transform.function = "log",
                 arima.model = "(0 1 1)(0 1 1)",
                 slidingspans = "",
                 history = "",
                 series.span = "2004.1,"
                 )

fd.seats2 <- seas(x,
                  transform.function = "log",
                  arima.model = "(0 1 1)(0 1 1)",
                  slidingspans = "",
                  history = "",
                  series.span = "2004.1,"
                  )

udg(fd.x11, "r01.lag00.aar.all")    # 3.122053
udg(fd.x11sl, "r01.lag00.aar.all")  # 3.246875
udg(fd.seats, "r01.lag00.aar.all")  # 3.667003
udg(fd.seats2, "r01.lag00.aar.all") # 3.667003 

udg(fd.seats2, "MM.AAR")
# and then the fd.seats2 has  the lowest revisions,
# 2.78 SA and 1.63 MM,

# while fd.seats has the worst,
# 3.56 SA and 2.05 MM,

# with the X-11 adjusments in the middle,
# fd.x11:   3.16 SA, 1.92 MM
# fx.x11sl: 3.03 SA, 1.77 MM


### Question 8 ----

# Create graphs comparing the seasonal adjustments of 
# fd.x11, fd.x11sl, fd.seats, and fd.seats2.
# Are they similar?
# Look at overlay graphs of the seasonal factors, using the
# sfcompgraph() function.
# Where do you see the biggest differences?

# sfcompgraph(m1, m2, sf1 = "sf", sf2 = "sf")
# Overlays the seasonal factors of the seasonal objects m1 and m2.
# By default the purely seasonal factors (d10 or s10) will be plotted;
# to instead plot the combined seasonal factors (d16 or s16) of m1 and
# /or m2, set sf1 = "cf" and/or sf2 = "cf"
sfcompgraph(m1 = fd.x11, m2 = fd.x11sl, sf1 = "sf", sf2 = "sf")
# Barely any difference

sfcompgraph(m1 = fd.seats, m2 = fd.seats2, sf1 = "sf", sf2 = "sf")
# Barely any difference

sfcompgraph(m1 = fd.x11, m2 = fd.seats, sf1 = "sf", sf2 = "sf")
# Clearly different

sfcompgraph(m1 = fd.x11sl, m2 = fd.seats2, sf1 = "sf", sf2 = "sf")
# Clearly different

# Answer:
# The two X-11 adjustments are very similar and the two
# SEATS adjustments are very similar.

# The largest differences are between the X-11 and the SEATS adjustments.

# END

## Tables of X-13ARIMA-SEATS Output ----

# - A: Prior adjustments (regARIMA model) before seasonal adjustment
# - B: X-11 preliminary estimation of seasonal, trend, and extreme value weights
#   - B1: Prior-adjusted original series, starting point for B iteration
#   - B2: Trend estimate TC_t = 2x12-MA(Y_t)
#   - B3: De-trended series SI_t = Y_t / TC_t, B3 = B1 / B2
#   - B4: Replacement of preliminary extreme values
#   - B5: Preliminary seasonal factors from SI ratios, S_t = s3x3-MA(SI_t)
#   - B6: Estimate of seasonally adjusted series A_t = Y_t / S_t, B6 = B1 / B5
#   - B7: Trend estimate, Henderson filter applied to B6
#   - B8: De-trended series, B8 = B1 / B7
#   - B9: Repeats steps of B4 using B8 values to get de-trended and extreme value adjusted series
#   - B10: Estimated seasonal factors, s3x5-MA of B8 or B9
#   - B11: Estimated seasonally adjusted series, B11 = B1 / B10
#   - B13: Estimated irregular component, B13 = B11 / B7
#   - B17: Preliminary extreme value weights
#   - B20: Adjustment values for extreme values, B20 = B13 / [1 + B17*/(B13 - 1)] or B20 = B13*(1 - B17)
# - C: X-11 intermediate estimation of seasonal, trend, plus final estimation of extreme value weights
#   - C1: Prior-adjusted series also adjusted for preliminary extreme value weights, starting point of C iteration, C1 = B1 / B20 if B17 < 1, else C1 = B1
#   - C2: Trend estimate TC_t = 2x12-MA
#   - C3: De-trended series SI_t = Y_t / TC_t, C4 = C1 / C2
#   - C5: Preliminary seasonal factors from SI ratios, S_t = s3x3-MA(SI_t)
#   - C6: Estimate of seasonally adjusted series A_t = Y_t / S_t, C6 = C1 / C5
#   - C7: Trend estimate, Henderson filter applied to C6
#   - C9: De-trended series, C9 = C1 / C7
#   - C10: Estimated seasonal factors, s3x5-MA of C9
#   - C11: Estimated seasonally adjusted series, C11 = B1 / C10
#   - C13: Estimated irregular component, C13 = C11 / C7
#   - C17: Final weights for extreme value weights
#   - C20: Adjustment values for extreme values, C20 = C13/[1 + C17*(C13-1)] or C20 = C13*(1-C17)
# - D: X-11 final estimation of components
#   - D1: Prior-adjusted series, with an adjustment for  final extreme value weights B1/C20 or B1-C20, Start of D iteration, D1 = B1 / C20 if C17 < 1, else D1 = B1
#   - D2: Trend-estimate TC_t = 2x12-MA
#   - D4: De-trended series SI_t = Y_t / TC_t, D4 = D1 / D2
#   - D5: Preliminary seasonal factors from SI ratios, S_t = s3x3-MA(SI_t)
#   - D6: Estimate of seasonally adjusted series, A_t = Y_t / S_t, D6 = D1 / D5 
#   - D7: Trend estimate, Henderson filter applied to D6
#   - D8: Un-adjusted SI ratios D8 = B1 / D7 or D8 = B1 - D7
#   - D9: Final replacement values for SI ratios at extreme values D9 = D1 / D7 or D9 = D1 - D7
#   - D10: Seasonal factors
#   - D11: Final seasonally adjusted series modified for extreme values and smoothed with Henderson trend filter
#   - D12: Final Henderson trend
#   - D13: Final irregular component, D13 = D11 / D12
#   - D16: Final combined factors, D11 = A1 / D16 or D11 = A1 - D16
# - S: SEATS seasonal adjustment components

# - E: Additional X-11 tables
#   - E18: Final adjustment ratio E18 = A1 / D11
# - F: X-11 seasonal adjustment diagnostics

# - G: Spectral diagnostics
# - R: Revision history diagnostics
# - S: Sliding span diagnostics

# END