# Exercise 3 - regARIAM models ----

## Prep ----
if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

# Load the monthly retail sales at automobile dealerships.
f  <- "data/retail/Automobile dealers.dat"
ad <- import.ts(file = f, format = "datevalue")

plot(ad)
graphics.off()

# Adjust the series with:
# - a test for log transformation,
# - a test for trading day and easter, -> standard c("td", )
# - automatic model selection,
# - and tests for AO, LS, and TC outliers -> outlier.types = "all"
m <- seas(ad,
          x11 = "", 
          regression.aictest = c("td", "easter"), # (default)
          outlier.types = "all" # c("ao", "ls", "tc")
          )

summary(m)

# Use the udg() function to answer the following questions:

## Question 1 ----
# What ARIMA model was chosen for this series? Is it a mixed model?
summary(m)$model$arima$model  # (0 1 1)(0 1 1)
summary(m)$transform.function # "log"
transformfunction(m) # "log"
m$spc$transform
# Correct answer with udg() function:
udg(m, "arimamdl") # (0 1 1)(0 1 1)
udg(m, "aictrans") # "Log(y)"
# This is the classic airline model and it is not a mixed model because both
# first differences are taken, seasonal and non-seasonal.


# When a seasonal object is created with the automdl spec activated,
# the AIC test tables for trading day and Easter day are printed in the
# output file.
# We can hard-code the previously selected ARIMA model and run the seas() 
# function again.
m_arima <- seas(ad,
                x11 = "",
                regression.aictest = c("td", "easter"),
                outlier.types = "all",
                arima.model = "(0 1 1)(0 1 1)",
                transform.function = "log"
                )

summary(m_arima)

## Question 2 ----
# Did X-13ARIMA-SEATS find the Easter holiday regressor to be significant?
# If yes, what is this regressor's t-value?
data.frame(summary(m_arima))
# X-13ARIMA-SEATS chose a model without an Easter holiday regressor.
# AICc with aicdiff = 0.0000 prefers model without Easter regressor.
udg(m_arima, "testalleaster") # "no"
udg(m_arima, "aictest.easter.num") # 3

udg(m_arima, "aictest.e.aicc.noeaster") # 4936.17
udg(m_arima, "aictest.e.aicc.easter01") # 4937.915

# A variable is only added to the model if:
# AICCwith + threshold < AICCwithout
4937.915 + 0 < 4936.17 # FALSE

# Including an Easter holiday regressor **increases** the AICc by 1.745,
# whereas the goal is to find the lowest possible AICc!
udg(m_arima, "aictest.e.aicc.easter08")
udg(m_arima, "aictest.e.aicc.easter15")

udg(m_arima, "aictest.easter.reg") # "easter"

# We find the result of the AIC-test for Easter directly:
udg(m_arima, "aictest.e") # "no"

# The difference between the model is:
udg(m_arima, "aictest.diff.e") # -1.745611 
# The cutoff for the Easter holiday regressor is 0.00 

## Question 3 ----
# Did X-13ARIMA-SEATS find a significant trading day regressor?
summary(m_arima)
# Answer: The trading day regressors are small in size and insignificant,
# except for the Thursday regressor which is significant at an 0.05 level.
udg(m_arima, "aictest.td.reg2") # "td1coef"
udg(m_arima, "aictest.td.aicc.notd") # 5013.81


udg(m_arima, "aictest.td.aicc.td") # 4936.17
5013.81 - 4936.17 # 77.64

udg(m_arima, "aictest.td.aicc.td1coef") # 4957.274
5013.81 - 4957.274 # 56.536

# Including all six trading day regressors reduces the AICc
# by 77.64. Including the combined regressors reduces the AICc
# only by 56.536, hence the model with all six trading day 
# regressors is chosen.

# We can directly ask if the aictest chose the trading day regressors:
udg(m_arima, "aictest.td") # "td"
# The model with all six trading day regressors is chosen.

udg(m_arima, "aictest.diff.td") # 77.64027

## Question 4 ----
# Were any outliers found?
# If so, what are they and what are their t-values?
summary(m_arima)
# Coefficients:      Estimate   Std. Error z value Pr(>|z|)  
# TC2001.Oct         0.255785   0.026956    9.489   < 2e-16 ***
# AO2005.Jul         0.116865   0.023215    5.034  4.81e-07 ***
# LS2008.Oct        -0.148250   0.027615   -5.368  7.94e-08 ***
# AO2009.Aug         0.141165   0.023253    6.071  1.27e-09 ***

as.data.frame(summary(m_arima))

# There are two additive outliers that are highly significant,
# one in July 2005 and one in August 2008.

# There is one significant level shift in October 2008.

# There is one significant trend change in Ocotober 2001.

## Question 5 ----
# What was the estimate of the seasonal MA parameter?
m_arima$model$arima$ma[2] # 0.6103379

## Question 6 ----
# Based on these results, would you consider a regARIMA model of the form
# ARIMA(p d q)(P D Q) + fixed_seasonal_regressors? Why and why not?

# Answer: No. Fixed seasonal regressors should only be used if the seasonal
# pattern is *very* stable.
# Stable seasonal patterns have a seasonal MA parameter close to 1!


# hard-code ARIMA model 
# We have found the outliers in the past, and since they are unlikely to change,
# we hard-code them.

# First we only hard-code the ARIMA(0 1 1)(0 1 1)[12] (Airline) model:
m_arima <- seas(ad, 
                outlier.types = "all", 
                x11 = "", 
                arima.model = '(0 1 1)(0 1 1)')
out(m_arima)
summary(m_arima)

# We can hard-code the model:
static(m)

m_static <- seas(ad,
                 x11 = "",
                 regression.variables = c("td", "tc2001.Oct", "ao2005.Jul", "ls2008.Oct", "ao2009.Aug"),
                 arima.model = "(0 1 1)(0 1 1)",
                 regression.aictest = NULL,
                 transform.function = "log"
                 )

summary(m_static)
as.data.frame(summary(m_static))

# END