# Exercise 2 - ARIMA models ----

# DETAILS
# Modes of seasonal adjustment: 
# In any X-13ARIMA-SEATS seasonal adjustment, the original time series (O)
# is decomposed into three basic components:
# - Trend-Cycle (C): Medium-to-long term movements of the series, including consequential turning points.
# - Seasonal (S):    Within-year fluctuations about the trend that recur in the same month or quarter from year to year.
# - Irregular (I):   Residual component remaining after seasonal, trend (and trading day and holiday effects) are removed from the series. 
#   It is characterized by movements of very short duration. 
#   These can be quite large if there are strikes or other unusual economic events of short duration.

# The components are related in the following way
# Multiplicative: O = C * S * I, SA = C * I
# Additive:       O = C + S + I, SA = C + I

## Prep ----
if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

## General information ----
# Many questions in this exercise can be answered using the udg() function:
# udg(seas_object, "udg_name")

# There is an HTML document that lists the udg names
# Path: file:///C:/Users/matth/OneDrive/Dokumente/R-Scripts/CR-SAclass/software/R%20seasonal/UDG%20codes%20for%20WinX13%20Diagnostic%20Tables.html

# There is also the lookup table SPECS
head(SPECS)

# For examle, udg(m, "arimamdl") will give the ARIMA model.
m <- seas(AirPassengers)
m$model$arima$model # (0 1 1)(0 1 1)
udg(m, "arimamdl")  # (0 1 1)(0 1 1)

## Load the data ----

# Load the quarterly unemployment rate in Mexico:
f   <- "data/mx/National Unemployment Rate.dat"
nur <- import.ts(file = f, format = "datevalue")

plot(nur)

# Create a "seas" object called "amd".
# Use the default settings for x-11
# X-11 will automatically:
# - Test for log-transformation or none
# - Use default outlier identification
# - Test for Easter and trading day effects
# - Adjust the series with X-11 iterative moving averages

# We want to use a 3x3 seasonal filter:
amd <- seas(nur, 
            regression.aictest = NULL,
            x11.seasonalma = "s3x3"
            )
# seasonalma the seasonal moving average (seasonal filter) used to
# filter out the seasonal components 
# Here we fix a 3x3 seasonal moving average to extract the seasonal component.
nur1 <- stats::filter(nur,  filter = 1/3*c(1, 1, 1), method = "convolution", sides = 2)
nur2 <- stats::filter(nur1, filter = 1/3*c(1, 1, 1), method = "convolution", sides = 2)

ts.plot(nur, nur1, nur2, final(amd), col = c("black", "blue", "red", "green"))

# From the X-13ARIAM-SEATS manual - CHAPTER 7. - DOCUMENTATION FOR INDIVIDUAL SPECS 

# seasonalma: the seasonal n x m moving average (seasonal filter) to estimate the seasonal factors. 
# An n-term simple average is taken of a sequence of consecutive m-term simple averages.
# If no seasonal moving average is specified, the program will choose the seasonal filter automatically; 
# this option can also be invoked by setting seasonalma = msr. 
# In the simplest version of X-11, a 3x3 moving average was used to calculate the initial seasonal
# factors in each iteration, and a 3x5 moving average to calculate the final seasonal factors.

# Get the adjustment summary:
summary(amd)
as.data.frame(summary(amd))
udg(amd, "arimamdl")
amd$model$arima$model

amd.summary <- summary(amd)
amd.summary$transform.function # "log"
amd.summary$model$arima$model # "(1 0 2)(1 1 0)"
# Transform: log
# This is a mixed model: ARIMA: (1 0 2)(1 1 0)
# Mixed, because we take the first seasonal difference but not the first non-seasonal 
# difference.

# Or view the HTML output:
out(amd)

## Question 1 ----
# What is the transformation of choice?
# Answer: log-transformation
summary(amd)$transform.function # "log"

## Question 2 ----
# What ARIMA model was selected?
summary(amd)$model$arima$model # (1 0 2)(1 1 0)
# This is a mixed model, with one seasonal difference but no non-seasonal difference.

# Force X-11 not to use a mixed model:
amd_nomix <- seas(nur, 
                  regression.aictest = NULL,
                  x11.seasonalma = "s3x3",
                  automdl.mixed  = "no"
                  )

# Check the summary statistics
summary(amd_nomix)$model$arima$model  # (1 0 0)(1 1 0)
summary(amd_nomix)$transform.function # "log"
# This is still a mixed model with one seasonal difference but no nonseasonal difference
# It can happen that automdl chooses a mixed model even though we asked to exclude those.

# Next, we restrict the model selection to models with both,
# first seasonal and non-sesonal differences:
amd_dfif11 <- seas(nur,
                   regression.aictest = NULL,
                   x11.seasonalma = "s3x3",
                   automdl.diff = "(1 1)"
                   )

# Check the summary statistics
summary(amd_dfif11)

## Question 4 ----
# What ARIMA model was selected?
summary(amd_dfif11)$model$arima$model # (0 1 0)(0 1 1)
# Finally, we don't have a mixed model anymore.

# Look at the graphs of the original series and the logs of the original series:
par(mfrow = c(1, 2))
plot(nur)
plot(log(nur))
graphics.off()

# From these graphs, it is not obvious that the log-transformation
# makes the data look more "normal" than before.

# Open the output file of amd with out(amd):
out(amd)

# In the output file, look for the AIC test for the transformation.

## Question 5 ----
# What is the AICc of the series with the log transformation?
# Without transformation? Which is lower?

# We create another model without a log-transformation
amd_notransf <- seas(nur,
                     regression.aictest = NULL,
                     x11.seasonalma = "s3x3",
                     transform.function = "none"
                     )

summary(amd_notransf)$transform.function # "none"
summary(amd_notransf)$model$arima$model  # (0 1 0)(0 1 1) not a mixed model
udg(amd_dfif11, "aicc")   # 19.55917 
udg(amd_notransf, "aicc") # 18.72994 
# The AICc for the series without a log-transformation is lower.

# Then why did X-13ARIMA-SEATS not chose the model without a log-transformation?

# Answer: The improvement in the AICc from not taking the log-transformation is too small,
# so the log-transformation is chosen.

udg(amd_dfif11, "aictest.trans.aicc.nolog") # 20.35245 
udg(amd_dfif11, "aictest.trans.aicc.log")   # 21.05798 

as.data.frame(udg(amd_dfif11))

udg(amd_dfif11, "aictest.trans.aicc.nolog")[[1]] - udg(amd_dfif11, "aictest.trans.aicc.log")[[1]]
# -0.7055216 < -2

# X-13 manual, chapter 7.18 TRANSFORM spec

# The aicdiff argument defines the difference in AICC needed to accept no transformation 
# when the automatic transformation selection option is invoked (function = auto). 
# The default value is aicdiff = -2.0 for monthly and quarterly series and 0.0 otherwise.

# X-13 manual, chapter 7.18 TRANSFORM spec - DETAILS

# For function = auto (and only positive values in the series, else log() cannot be applied),
# X-13 creates a regARIMA model for a log-transformed and an untransformed series.
# The log transformation will be favored unless
# AICCnolog - AICClog < -2
# The negative default threshold makes it more likely the log-transformation is chosen.
# This is a choice of convenience, since a log-transformation is usually appropriate for
# economic time series. 



## Question 6 ----
# What ARIMA model was selected for the model without a log-transformation?
summary(amd_notransf)$model$arima$model # (0 1 0)(0 1 1) not a mixed model


## Question 7 ----
# Compare the ARIMA parameters of this model with those of the model from amd_diff11. 
# Are they similar?

# From the manual: 
# Sum of MA$Seasonal$s$s to MA$Seasonal$s$Q, where s is the period and Q is the seasonal MA order times ss
# Here we have quarterly data so s = 4, and Q = 1*4=4 (first seasonal difference)
# hence we arrive at MA$Seasonal$04$04:
udg(amd_dfif11, "MA$Seasonal$04$04")   # 0.4961196
udg(amd_notransf, "MA$Seasonal$04$04") # 0.612299

summary(amd_dfif11)$model$arima$ma   # 0.4961196
summary(amd_notransf)$model$arima$ma # 0.612299
# Answer: The ARIMA parameters, that is the seasonal Theta, are somewhat different.


# Compare the sesasonal adjustments of amd, amd_diff11, and amd_notransf:
plot(final(amd))
lines(final(amd_diff11), col = "blue")
lines(final(amd_notransf), col = "red")
graphics.off()
# From the plot we see that taking the log-transformation of the data was not necessary.
ts.plot(final(amd), final(amd_dfif11), final(amd_notransf), col = c("black", "blue", "red"))
graphics.off()

## Question 8 ----
# Which adjustments are most similar? Most different?

# Answer: 
# amd and amd_diff11 are very similar,
# amd_notransf is the most different.
# Judging from the plot.

# END