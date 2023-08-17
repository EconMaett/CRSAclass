# Exercise 5 - regARIMA diagnostics ----

## Prep ----
if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)
# Import the data on retail sales from furniture stores
# and home furniture stores.

f <- "data/retail/Furniture and home furnishings stores.dat"
x <- import.ts(file = f, format = "datevalue")
plot(x)
graphics.off()

## Part 1 ----

# Create a "seas" object called "m1" using the default options

# By default, x11 will chose a transformation (log or none),
# test for trading day effects (td) and easter,
# look for additive outliers (ao) and level shifts (ls),
# and automatically choose an ARIMA(p d q)(P D Q) model.

# We include the arguments 
# - history.estimates = "fcst" and
# - history .start = "2010.Jan"
m1 <- seas(x, 
           x11 = "",
           history.estimates = "fcst",
           history.start = "2010.Jan"
           )

summary(m1)
# Transform: log
# ARIMA: (0 1 0)(0 1 1)

# Use the output file, the udg() function, or the view() facility to answer
# the following questions:


### Question 1 ----
# What ARIMA model did the program choose?
transformfunction(m1) # "log"
summary(m1)$model$arima$model # (0 1 0)(0 1 1)

### Question 2 ----
# Which regressors did the tests choose to include?
# Are they all significant?
summary(m1)
# Trading day effect regressors Monday to Saturday were included.
# Of these, only Friday was highly significant.
# An additive outlier for December 2000 is highly significant.

### Question 3 ----
# What was the AICc?
summary(m1)$aicc # 3432

udg(m1, "aape.0") # 2.106528
# aape.0: Average absolute percentage error of the forecasts 
# in the last three years. 

# An average of the 1-step ahead to 1-year ahead forecasts, 
# (e.g 12-step ahead for monthly and 4-step ahead for quarterly series)
# of the data with one, two and three years removed. 

# By default, this is calculated using within-sample forecasts,
# but it can be done with out-of-sample forecasts if requested.

udg(m1, "nlbq") # Number of the lags from 1 to 24 with significant Ljung-Box Q statistic.
udg(m1, "lblags") # List of lags with significant LBQ.
udg(m1, "nbpq") # Number of the lags from 1 to 24 with significant Box-Pierce Q statistic.
udg(m1, "bplags") # List of lags with significant BPQ.
udg(m1, "bplags") # List of the seasonal lags in bplags Seasonal lags with significant BPQ.
udg(m1, "sigacflags") # Lags with significant autocorrelation in the residuals.
udg(m1, "sigpacflags") # Lags with significant partial autocorrelation in the residuals.

# Peaks in the spectrum of the residuals
udg(m1, "spcrsd.s1")
udg(m1, "spcrsd.s2")
udg(m1, "spcrsd.s3")
udg(m1, "spcrsd.s4")
udg(m1, "spcrsd.s4")
udg(m1, "spcrsd.s5")
udg(m1, "spcrsd.t1")
udg(m1, "spcrsd.t2")


#### Question 4 ----
# Were any lags of the ACF or PACF significant?
udg(m1, "sigacflags") # Lags with significant autocorrelation in the residuals.

# Lags 2, 6, and 10 of the ACF

udg(m1, "sigpacflags") # Lags with significant partial autocorrelation in the residuals.
# Lags 2, 6, and 20 of the PACF


### Question 5 -----
# At how many lags was the Ljung-Box Q significant?

# Answer: 
udg(m1, "nlbq") # 15, Number of the lags from 1 to 24 with significant Ljung-Box Q statistic
udg(m1, "lblags") # List of lags with significant LBQ



## Part 2 ----

# Create a second model for this series called "m2" with:
# - the regressors that were previously selected hard-coded 
#   but insignificant regressors removed.
# - The previously selected transformation hard-coded
# - The ARIMA model (0 1 2)(0 1 1)
# - Outlier identification for AO and LS outliers
# - the arguments history.estimates = "fcst"
#   and history.start = "2010.Jan" and x11 = ""
summary(m1)
# "AO2000.Dec"

m2 <- seas(x, 
           x11 = "",
           regression.variables = c("AO2000.Dec"),
           history.estimates = "fcst",
           history.start = "2010.Jan",
           arima.model = "(0 1 2)(0 1 1)",
           outlier.types = c("ao", "ls")
           )

summary(m2)
# The two nonseasonal MA coefficients are insignificant.


### Question 1 ----
# Why might we think of changing the model to (0 1 2)(0 1 1)
# would be a good idea?

# Answer: The ACF and PACF were both significant at lag 2,
# indicating some residual autocorrelation after the fit to
# the (0 1 0)(0 1 1) model.
# We might try to remove this residual autocorrelation
# by taking another lag in the non-seasonal MA part.
acf(resid(m2))
pacf(resid(m2))
# This did not help it seems...

### Question 2 ----
# Look at the ARIMA model parameters. 
# Are they all significant?
summary(m2)
# No. The two non-seasonal MA-parameters are both insignificant.

# The MA-parameter at lag 1, Theta1, is 0.082527,
# which is between one and two standard errors from zero.

# This is not significant in the traditional statistical sense.
# Greater than two standard errors from zero,
# but it is large enough that we would not want to skip it in
# a skipped lag model.


### Question 3 ----
# Were any new outliers found?
summary(m2)
# Answer: No.

# Is it valid to use AICc to compare these two models?
# If yes, then which model is preferred?
# Answer: Yes, it is valid.
# The models m1 and m2 have the same 
# - model span,
# - outliers
# - order of differencing (d=1,D=1).
summary(m1)$aicc # 3431.514
summary(m2)$aicc # 3428.784
# The AIcc of model two is lower by 2.730865.
# Hence we prefer model m2.

### Question 4 ----
# Is it valid to use the average forecast error over the last
# three years to compare the two models?
# If yes, which model is preferred?
# Answer: It is always valid to use the average forecast error 
# over the past three years to compare two models.
udg(m1, "aape.0") # 2.106528
udg(m2, "aape.0") # 2.018939 
# The average forecast error over the last three years is 
# lower for the second model, hence we prefer model m2.


### Question 5 ----
# Were any lags of the ACF or PACF significant?
udg(m2, "sigacflags") # 4, 6, 10
udg(m2, "sigpacflags") # 4, 6, 10, 20

# Yes, lags 6 and 10 are significant.
# But since these are not multiples of the seasonality
# we do not worry about them.


### Question 6 ----
# At how many lags was the Ljung-Box Q significant?
udg(m2, "nlbq") #  9
# Answer: 9 lags.


## Part 3 ----

# Create an out-of-sample forecast error plot to compare the first
# and second models (m1 and m2) using the following commands:
h1 <- series(m1, "fce") # model 1: SumSqFcstError.01., SumSqFcstError.12.
h2 <- series(m2, "fce") # model 2: SumSqFcstError.01., SumSqFcstError.12.

diff1  <- h1[, 1] - h2[, 1] # Model difference in SumSqFcstError.01.
diff12 <- h1[, 2] - h2[, 2] # Model difference in SumSqFcstError.12

length(diff1)  # 65
length(diff12) # 65

tail(h2)
h2[length(h2[, 1]), 1] # Last and largest SumSqFcstError.01. = 1630129
h2[length(h2[, 2]), 2] # Last and largest SumSqFcstError.12. = 8922385


sdiff1  <- (diff1  * length(diff1))  / h2[length(h2[, 1]), 1]
sdiff12 <- (diff12 * length(diff12)) / h2[length(h2[, 2]), 2]

# To make the model differences in 1-month-ahead and
# 12-month ahead forecasts comparable,
# we divide both series by the largest error in model 2.

plot(sdiff1,
     ylim = range(sdiff1, sdiff12),
     main = "Squared forecat error differences, model 1 - model 2")
lines(sdiff12, col = "red")
abline(h = 0, col = "grey")
graphics.off()
# Out-of-sample forecast error plot

# Until about 2013, lead 12 strongly prefers the second model, as the errors get smaller,
# but they increase after 2013.
# Lead 1 increases over time, although it varies a lot.
# This would indicate a preference for model 2.

# Which of the two models do you prefer?
# Answer: On the whole, diagnostics seem best for the second model.

# END