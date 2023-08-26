library(seasonal)

# You can run demo(seas) for an introduction

# seas() is the core function of the seasonal package.
# By default, seas() calls the automatic procedures of X-13ARIMA-SEATS to perform
# a seasonal adjustment that works well in most circumstances.
m <- seas(AirPassengers)

# The first argument is a time series of class "ts".
class(AirPassengers) # "ts"
class(m) # "seas"
# It returns an object of class "seas" that contains all necessary information
# on the adjustment

# The final() function returns the adjusted series:
final(m)

# The plot() method shows a plot with the unadjusted and the adjusted series:
plot(m, trend = TRUE)
plot(m, trend = FALSE)
# The Trend and the adjusted series overlap
# Y = TC + S + I
# A = Y - S - I = TC
# or Y = TC * S * I
# A = Y / (S*I) = TC

# The summary() method allows you to display an overview of the model:
summary(m)
# An Airline model was chosen: ARIMA: (0 1 1)(0 1 1)[12], log-transformation
# There is a Weekday regressor,
# An Easter[1] regressor
# An additive outlier Ao1951.May due to a strike

# By default, seas() calls the SEATS adjustment procedure.
# If you prefer the X11 adjustment procedure, use:
seas(AirPassengers, x11 = "")

# Alternatively, all inputs may be entered manually:
seas(x = AirPassengers,
     regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
     arima.model = "(0 1 1)(0 1 1)", 
     regression.aictest = NULL,
     outlier = NULL, 
     transform.function = "log")
# This is the default Airline model

# The static() command reveals the static call from above that is needed to 
# replicate the automatic seasonal adjustment procedure:
static(m)

# Invoke X-13ARIMA-SEATS options as "spec.argument" through the "..." argument:

# no test for Easter effects, only trading day effects
seas(AirPassengers, regression.aictest = c("td"))

# force equality of annual values
seas(AirPassengers, force.type = "denton")

# all series from X-13 can be extracted with series()
ts.plot(series(x = m, series = "forecast.forecasts"))

# use genhol to generate user defined holiday variables:
data(seasonal) # exports and imports of China
data(holiday) # dates of Chinese New Year and Easter

# Create dummy variables for six days after Chinese New Year
cny.ts <- genhol(cny, start = 0, end = 6, center = "calendar")

# the series can be used as a user defined regressor
plot(cny.ts)

seas(imp, 
     xreg = cny.ts, 
     regression.usertype = "holiday", 
     x11 = "",
     regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
     arima.model = "(0 1 2)(0 1 1)", 
     regression.aictest = NULL,
     outlier = NULL, 
     transform.function = "log")

# Introduction to Seasonal ----

## Getting started ----
# seas() is the core function of the seasonal package.
# seas() calls the automatic procedures of X-13ARIMA-SEATS
m <- seas(AirPassengers)

# The first argument of seas() is a time series of class "ts".
# The function returns an object of class "seas" that contains
# all the necessary information on the adjustment.

# There are several methods for "seas" objects:
# final() returns the adjusted series.
# plot() shows the adjusted and unadjusted series.
# summary() provides an overview of the model.
final(m)
plot(m)
summary(m)

# By default, seas() calls the SEATS adjustment procedure.
# If you prefer the X11 adjustment procedure, 
# use:
seas(AirPassengers, x11 = "")

# A default call to seas() invokes the following automatic
# procedures of X-13ARIMA-SEATS:
# - Transformation selection (log / no log)
# - Detection of trading day and Easter effects
# - Outlier detection
# - ARIMA model search

# Alternatively, all inputs may be entered manually:
seas(x = AirPassengers, 
     regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
     arima.model = "(0 1 1)(0 1 1)",
     regression.aictest = NULL,
     outlier = NULL,
     transform.function = "log")

# The static() method returns the manual call of the model:
static(m)


# If you have "Shiny" installed, the inspect() command
# offers an easy way to analyze and modify a seasonal
# adjustment procedure:
library(shiny)

inspect(m)

## Input ----

# You can use the syntax of X-13ARIMA-SEATS with
# the "..." argument in seas().

# The X-13ARIMA-SEATS syntax uses "specs" and "arguments",
# with each spec optionally containing some arguments.

# These spec-argument combinations can be added to seas() by
# separating the spec and the argument by a dot (.).

# In order to set the "variables" argument of the 
# "regression" spec equal to "td" (trading day adjustment),
# use:
m <- seas(AirPassengers, regression.variables = c("td", "ao1955.jan"))
m


# Note that R vectors may be used as an input.
# If a spec is added without any arguments, the spec should be set
# equal to an empty string (""), or , alternatively,
# to an empty list() as in earlier versions.

# Several defaults of seas() are empty strings, such as the default
# seats = "".

# Check out the help page 
?seas

# Note the difference between seats = "" and seats = NULL.
# seats = "": means the spec is enabled but has no arguments.
# seats = NULL: means the spec is disabled.


# Translate the following model from the X-13ARIMA-SEATS manual:
# series { title = "Quarterly Grape Harvest" start = 1950.1
#       period = 4
#       data = (8997 9401 ... 11346) }
# arima { model = (0 1 1) }
# estimate {}

# translates to R:
seas(AirPassengers,
     x11 = "",
     arima.model = "(0 1 1)"
)

# seas() takes care of the "series" spec, and no input beside the
# time series has to be provided.

# As seas() uses the SEATS procedure by default, the use of X11 has to be
# specified manually.

# When the "x11" spec is added as an input, the mutually exclusive and 
# default "seats" spec is automatically disabled.

# With "arima.model", an additional spec-argument is added to the input
# of X-13ARIMA-SEATS.

# As the spec cannot be used in the same call as the "automdl" spec,
# the latter is disabled.

# There are some mutually exclusive specs in X-13ARIMA-SEATS.
# If more than one mutually exclusive spec is included in seas(),
# specs are overwritten according to the following priority rules:

# - Model selection
#   1. arima
#   2. pickmdl
#   3. automdl (default)

# - Adjustment procedure
#   1. x11
#   2. seats (default)

# As an alternative to the "..." argument, spec-arguments can also be supplied
# as a named list:
seas(list = list(x = AirPassengers, x11 = ""))


## Output ----

# With the series() function, it is possible to import almost all output
# that can be generated by X-13ARIMA-SEATS.

# The following command returns the forecasts of the ARIMA model as a "ts" object:
m <- seas(AirPassengers)
series(x = m, series = "forecast.forecasts")

# Because the forecast.save = "forecasts" argument has not been specified in
# the model call, series() re-evaluates the call with the "forecast" spec enabled.

# It is also possible to return more than one output table at the same time:
m <- seas(x = AirPassengers, x11 = "")
series(x = m, series = c("forecast.forecasts", "d1"))

# You can either use the unique short names of X-13, such as "d1",
# or the long names, such as "forecasts".

# Because the long table names are not unique, they need to be combined
# with the spec name ("forecast") as "forecasts.forecast".
# See ?series for a complete list of options:
?series

# Note that re-evaluation doubles the voerall computation time.
# If oyu want to speed it up, you have to explicitly state that in the model call:
m <- seas(AirPassengers, forecast.save = "forecasts")
series(x = m, series = "forecast.forecasts")
# This was faster!

# Some specs, like "sidingspans" and "history" are time consuming.
# Re-evaluation allows you to separate these specs from the basic model call:
m <- seas(AirPassengers)
series(m, "history.saestimates")
series(x = m, series = "slidingspans.sfspans")

# If you are using the HTML version of X-13, the out() function
# shows the content of the main output in the browser:
out(m)

## Graphs ----

# The plot() method draws the seasonally adjusted and the unadjusted series,
# as well as outliers.
m <- seas(AirPassengers, regression.aictest = c("td", "easter"))
plot(m)

# Optionally, add the trend:
plot(m, trend = TRUE)

# The monthplot() method allows for a monthwise
# or a quarterwise plot of the seasonal
# component and the SI component:
monthplot(m)
monthplot(m, choice = "irregular")

# Many standard R functions can be used
# to analyze a "seas" object:
pacf(resid(m))
# No significant autocorrelations
# in the residual

spectrum(diff(resid(m)))

plot(density(resid(m)))
# Residuals are normally distributed
# around zero.

qqnorm(resid(m))
# QQ-Plot indicates a normal distribution
# of the residuals.

# The identify() method can be used to select
# or deselect outliers by point and click.
identify(m)


## Inspect ----

# The inspect() function is a graphical tool for choosing a seasonal
# adjustment model using Shiny.
if (!require(shiny)) { install.packages("shiny") }
library(shiny)

# deprecated: inspect(m)
view(m) # same structure as the website

# Frequently used options can be modified using the drop down selectors
# in the upper left panel.

# Each change results in a re-estimation of the seasonal adjustment model.

# In the R-call, the output and summary are updated accordingly.

# Alternatively, the R-call can be modified manually in the lower left panel.

# Press "Run Call" to re-estimate the model and to adjust the option selectors,
# the output, and the summary.

# With the "Clsoe and Import" button, view() is closed and the call is
# imported to R.

# The "static" button subsitutes automatic procedurs by the 
# automatically chosen spec-argument options, just like static().

# The views in the upper rith panel can be selected from the
# drop down menu.

# The views can also be customized.
# See ?view
?view

# You can store the model after closing the GUI,
# for later use:
m <- seas(AirPassengers)
view(m)

m.upd <- view(m)


# The seasonalview package offers and extended GUI with the same
# look and feel as the website.

## Chinese New Year, Indian Diwali and customized holidays ----

# seasonal includes genhol(), a function that makes it easy to model
# user-defined holiday regression effects.

# genhol() is an R replacement for the equally named software by the
# US Census Office.

# The function uses an object of class "Date" as its first argument, which specifies
# the occurrence of the holiday.

# In order to adjust Indian industrial production for Diwali effects:
# variables included in seasonal:
# iip: Indian industrial production
# cny, diwali, easter: dates of Chinese New Year, Indian Diwali and Easter
seas(iip, 
     x11 = "", 
     xreg = genhol(diwali, start = 0, end = 0, center = "calendar"),
     regression.usertype = "holiday")

# For more examples, including Chinese New Year and complex pre- and
# post-holiday adjustments, see
?genhol
# GenHol = Generate Holiday Regression Variables

# Replacement for the genhol software by the U.S. Census Bureau,
# a utility that uses the same procedure as
# X-12-ARIMA to create regressors for the U.S. holidays of
# Easter, Labor Day, and Thanksgiving.

# First argument x is a vector of class "Date",
# containing the occurrences of the holiday.
# can be generated with as.Date().

# start: integer, shifts the start point of the holiday.
# Use negative values if start is before the specified date.

# end: integer, shifts the end point of the holiday.
# Use negative values if end is before the specified date.

# frequency: integer, frequency of the resulting series.

# center: Either "calendar", "mean", or "none" (default).
# Centering avoids a bias in the resulting series.
# Use "calendar" for Easter or Chinese New Year,
# "mean" for Ramadan.

# The resulting time series can be used
# as a user defined variable in seas().

# Usually, you want the holiday effect to be removed
# from the final series, so you specify
# regression.usertype = "holiday".
# The default is to include user defined variables in the final
# series.

# Dates of Chinese New Year, Indian Diwali, and Easter:
data(holiday)
easter
cny
diwali

# 10 day before Easter day to one day after, quarterly data:
genhol(x = easter, start = -10, end = 1, frequency = 4)
genhol(x = easter, frequency = 2) # easter is always in the first half-year

# centering for overall mean or monthly calendar means
genhol(x = easter, center = "mean")
genhol(x = easter, center = "calendar")


### replicating X-13's built-in Easter adjustment

# built-in
m1 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log", x11 = "")

summary(m1)

# user defined variable
ea1 <- genhol(x = easter, start = -1, end = -1, center = "calendar")

# regression.usertype = "holiday" ensures that the effect is 
# removed from the final series.
m2 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea1, regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log", x11 = "")
summary(m2)

all.equal(target = final(m2), current = final(m2), tolerance = 1e-06)
# TRUE

# with genhol, it's possible to do slightly better, by adjusting the
# length of easter from Firday to Monday:
ea2 <- genhol(x = easter, start = -2, end = +1, center = "calendar")
m3 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea2, regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log", x11 = "")

summary(m3)


## Chinese New Year
data(seasonal)
data(holiday) # dates of Chinese New Year, Indian Diwali and Easter

# de facto holiday length is longer :)
cny.ts <- genhol(x = cny, start = 0, end = 6, center = "calendar")

m1 <- seas(x = imp, xreg = cny.ts, regression.usertype = "holiday", x11 = "",
           regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
           arima.model = "(0 1 2)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")

summary(m1)

# compare to identical no-CNY model
m2 <- seas(x = imp, x11 = "",
           regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
           arima.model = "(0 1 2)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")

summary(m2)

ts.plot(final(m1), final(m2), col = c("red", "black"))

# modeling complex holiday effects in Chinese imports
# - positive pre-CNY effect
# - negative post-CNY effect
pre_cny  <- genhol(x = cny, start = -6, end = -1, frequency = 12, center = "calendar")
post_cny <- genhol(x = cny, start =  0, end =  6, frequency = 12, center = "calendar")
m3 <- seas(x = imp, x11 = "",
           xreg = cbind(pre_cny, post_cny), regression.usertype = "holiday",
           x11 = list())


summary(m3)

ts.plot(final(m1), final(m2), final(m3), col = c("red", "blue", "black"))

# The standard procedure in blue is the worst...

### Indian Diwali (thanks to Pinaki Mukherjee)

# adjusting Indian industrial production
m4 <- seas(x = iip, x11 = "",
           xreg = genhol(x = diwali, start = 0, end = 0, center = "calendar"),
           regression.usertype = "holiday")

summary(m4)
ts.plot(iip, final(m4), col = c("black", "red"))

# without specification of 'regression.usertype', Diwali effects are
# added back to the final series
m5 <- seas(x = iip, x11 = "", 
           xreg = genhol(x = diwali, start = 0, end = 0, center = "calendar"))

ts.plot(final(m4), final(m5), col = c("red", "black"))

# plot the Diwali factor in Indian industrial production
plot(series(x = m4, series = "regression.holiday"))


### Using genhol to replicate the regARIMA estimation in R

# Easter regressor
ea <- genhol(x = easter, start = -1, end = -1, center = "calendar")
ea <- window(ea, start = start(AirPassengers), end = end(AirPassengers))

# estimating ARIMA model in base R
arima(x = log(AirPassengers), 
      order = c(0, 1, 1), 
      seasonal = c(0, 1, 1),
      xreg = ea)

summary(seas(AirPassengers, regression.variables = c("easter[1]"),
             regression.aictest = NULL))

# You get the same result as with the regARIMA model.

# Note that R defines the ARIMA model with negative signs before the MA term,
# X-13 with a positive sign.


## Production use ----

# seasonal offers a quick way to adjust time series in R, but it can
# also be used to process a large number of time series.

# There are two kinds of seasonal adjustment in production use:
# 1. A periodic application of an adjustment model to a time series
# 2. An automated adjustment to a large number of time series.

# This section shows how both tasks can be accomplished with seasonal and 
# base R:


## Storing calls and batch processing 

# seas() calls are R objects of the standard class "call".

# Like any R object, calls can be stored in a list() object.

# In order to extract the call of a "seas" object, you can access
# the $call element or extract the static call with static():

# two different models for two different time series
m1 <- seas(x = fdeaths, x11 = "")
m2 <- seas(x = mdeaths, x11 = "")

l <- list()
l$c1 <- static(m1) # static call (with automated procedures substituted)
l$c2 <- m2$call # original call


# Details under ?static:
?static
# In a static call, the default automatic procedures in the model call
# are subsituted by the choices they made.

l
l$c1

l$c2

# The list can be stored and re-evaluated if new data becomes available:
ll <- lapply(X = l, FUN = eval)

# Which returns another list containing the re-evaluated "seas" objects.

# If you want to extract the final series, use:
do.call(what = cbind, args = lapply(X = ll, FUN = final))

# Of course, you can also extract any other series, e.g.:
# seasonal component of an X-11 adjustment, see ?series


do.call(what = cbind, args = lapply(X = ll, FUN = series, "d10"))


?series
# The series() function imports all tables that can be 
# saved in X-13ARIMA-SEATS

m <- seas(AirPassengers)
series(x = m, series = "fct") # re-evaluate with the forecast spec activated

# more than one series
series(x = m, series = c("rsd", "fct"))

# "rsd": estimate.residuals
# "fct": forecast.forecasts
# "d1": x11.adjoriginald

# using long names
series(x = m, series = "forecast.forecasts")
series(x = m, series = "history.sfestimates")
series(x = m, series = "history.saestimates")
series(x = m, series = c("history.sfestimates", "history.trendestimates"))

# slidingspans spec
series(x = m, series = "slidingspans.sfspans")
series(x = m, series = "slidingspans.ychngspans")

# fundamental identities of seasonal adjustment
# Y = T * I * (S * TD)
all.equal(
  target = AirPassengers, 
  current = series(x = m, series = "seats.trend") *
    series(x = m, series = "seats.irregular") * series(x = m, series = "seats.adjustfac")
  )
# TRUE

# Y_sa = Y / (S * TD)
all.equal(target = final(m), current = AirPassengers / series(m, "seats.adjustfac"))
# TRUE

# Some X-13ARIMA-SEATS functions can be replicated in R:

# X-13ARIMA-SEATS spectrum
plot(series(m, "spectrum.specorig")[, -1], t = "l")
plot(x13.pacf[, 1], t = "h")

# R equivalent: pacf from stats
pacf(AirPassengers, lag.max = 35)

# use with composite (see vignette("multiple", "seasonal"))
vignette("multiple", "seasonal")

m_composite <- seas(x = cbind(mdeaths, fdeaths),
                    composite = list(),
                    series.comptype = "add")

series(x = m_composite, series = "composite.indseasadj")

# Multiple adjustments can be performed by supplying multiple time series
# as an "mts" object:
m <- seas(cbind(fdeaths, mdeaths), x11 = "")
final(m)

# This will perform two seasonal adjustments, one for fdeaths and one for mdeaths.
# X-13 spec-argument combinations can be applied in the usual way, such as x11 = "".
# If entered that way, it will apply to both series.

# We can use the list() argument to specify spec-argument pairs:
seas(x = cbind(fdeaths, mdeaths), list = list(x11 = ""))

# It is possible to specify individual specs for each series, by encapsulating specific
# spec lists in the list() argument.

# Adjust fdeaths by X-11 and mdeaths by SEATS (default) procedure:
seas(x = cbind(fdeaths, mdeaths),
     list = list(
       list(x11 = ""),
       list()
     ))

# We can even combine these ideas.
# Turn off the AIC test of the regression spec for both series (regression.aictest = NULL)
# and use X-11 to adjust fdeaths and SEATS to adjust mdeaths
seas(x = cbind(fdeaths, mdeaths), regression.aictest = NULL,
     list = list(
       list(x11 = ""),
       list()
     ))

# Specifying multiple series

# Instead of using an "mts" object as input, we can use a list of single "ts" objects:
seas(x = list(mdeaths, AirPassengers))
# This is convenient if the series differ in length or frequency.

# With the tsbox package, you can create such lists of time series from any time series object.

# If the data is a data.frame:
library(tsbox)
dta <- ts_c(mdeaths = ts_df(mdeaths), AirPassengers = ts_df(AirPassengers))
head(dta)
class(dta) # "data.frame"

# In order to seasonally adjust all series in the data frame, we run:
seas(ts_tslist(dta))

# Finally, we can specify the data directly in the list of lists:
seas(
  list = list(
    list(x = mdeaths, x11 = ""),
    list(x = fdeaths)
  )
)

## Automated adjustment of multiple series
# X-13 can also be applied to a large number of series, usign automated
# adjustment methods.

# This can be accomplished with a loop or an apply function.

# It is useful to wrap the call to seas() in a try() statement;
# That way, an error will not break the execution.

# You need to develop an error handling strategy for these cases.
# You can either:
# - Drop these cases
# - Use these cases without adjustment
# - Switch to a different automated routine.

# collect data
dta <- list(fdeaths = fdeaths, mdeaths = mdeaths)

# loop over dta
# Defensive programming!
ll <- lapply(X = dta, FUN = function(e) try(expr = seas(x = e, x11 = "")))

# list failing models
is.err <- sapply(X = ll, FUN = class) == "try-error"
ll[is.err]
# named list()
is.err # FALSE FALSE
ll[!is.err]
# return final series of successful evaluations
do.call(what = cbind, args = lapply(X = ll[!is.err], FUN = final))


# If you have several cores and want to speed things up,
# the process is well suited for paralelization:

# a list with 100 time series
largedta <- rep(x = list(AirPassengers), times = 100)


library(parallel) # This is part of a standard R installation

# For Windows users, use parLapply:

# set up cluster
cl <- makeCluster(spec = detectCores())
cl # Socketcluster mit 12 Knoten auf System ‘localhost’

# load "seasonal" for each node
clusterEvalQ(cl = cl, expr = library(seasonal))

# export data to each node
clusterExport(cl = cl, varlist = "largedta")

# run in parallel
parLapply(cl = cl, X = largedta, fun = function(e) try(seas(e, x11 = "")))

# finally, stop the cluster
stopCluster(cl = cl)

# Import X-13 models and series

# Two experimental utility functions allow you to import ".spc" files
# and X-13 data files from any X-13 set-up.

# Simply locate the path of your X-13 .spc file, and the import.spc function
# 2ill construct the corresponding call to seas() as well as the calls for
# importing the data.

# importing the original X-13 example file
import.spc(file = system.file("tests", "Testairline.spc", package = "seasonal"))

# If data is stored outside the .spc file, as it usually will be, 
# the calls will make use of the import.ts() function,
# which imports arbitrary X-13 data files as R time series.
# See ?import.ts
?import.ts

# seasonal has been originally developed for the use at 
# the Swiss State Secretariat of Economic Affairs. 

# It has been greatly improved over time thanks to suggestions and 
# support from Matthias Bannert, Ronald Indergand and James Livsey, Brian Monsell, Pinaki Mukherjee, Bruno Parnisari, and many others. 
# I am especially grateful to Dirk Eddelbuettel 
# for the fantastic work on the x13binary package.
# © 2016 Christoph Sax

## X-13 Story 
library(x13story)
view(story = "https://raw.githubusercontent.com/christophsax/x13story/master/inst/stories/x11.Rmd")

## Exmaples of X-13ARIMA SEATS in R
# Examples from the official manual

# 7.1 ARIMA
# Example 1
seas(AirPassengers,
     x11 = "",
     arima.model = "(0 1 1)")
# or
seas(AirPassengers,
     x11 = "",
     arima.model = c(0, 1, 1))
# Remark: If x11 = "", the x11 spec is specified,
# and the default seats speci is disabled.
# ARIMA models may be specified as a character string or as a
# numeric vector

# Example 2
seas(AirPassengers, x11 = "", transform.function = "log",
     arima.model = "(2 1 0)(0 1 1)")

# Example 3
seas(AirPassengers, x11 = "",
     transform.function = "log",
     regression.variables = c("seasonal", "const"),
     arima.model = "(0 1 1)")

# Example 4
seas(AirPassengers, x11 = "",
     arima.model = "([2] 1 0)") # AR-Nonseasonal-02 

# Example 5
seas(AirPassengers, x11 = "", transform.function = "log",
     regression.variables = c("const"),
     arima.model = "(0 1 1)12") # MA-Seasonal-12
# Example 6
seas(AirPassengers, x11 = "", transform.function = "log",
     regression.variables = c("const", "seasonal"),
     arima.model = "(1 1 0)(1 0 0)3(0 0 1)")

# AR-Nonseasonal-01, AR-Period 3-03, MA-Seasonal-12

# Example 7
seas(AirPassengers, x11 = "", transform.function = "log",
     arima.model = "(0 1 1)(0 1 1)12",
     arima.ma = " , 1.0f")

# MA-Nonseasonal-01, MA-Seasonal-12

# Remarks: Because the first element in the arima.ma argument is empty,
# it has to be entered as a character string instead of a numeric vector.

# 7.2 AUTOMDL

# Example 1
seas(AirPassengers, x11 = "", regression.variables = c("td", "seasonal"))
# Jan-Nov, Constant, Mon-Sat, Easter[1], Ao1951.May,
# AR-Seasonal-12

# Remarks: If the x11 spec is specified, as an empty spec, the seats spec is disabled.

# Example 2
seas(AirPassengers, 
     x11 = "", 
     regression.variables = c("td"),
     automdl.diff = c(1, 1), 
     automdl.maxorder = "3, ")
# Mon-Sat, Easter[1], Ao1951.May (Strike?)
# MA-Seasonal-12

# Remark: Because the second element in the automdl.maxorder argument is empty,
# it has to be a character string instead of a numeric vector.

# Example 3
m <- seas(AirPassengers, 
          x11 = "",
          regression.aictest = c("td"),
          automdl.savelog = "amd")
out(m)
m
# Weekday, Ao1951.May, MA-Nonseasonal-01, MA-Seasonal-12
# With the HTML version, the log output can be analyzed in the browser
# with the out() function.
# Click on Log Entry in the sidebar.


# 7.3 CHECK

# Example 1
m <- seas(AirPassengers, x11 = "", arima.model = "(0 1 1)(0 1 1)", check.print = "all")
m

# Weekday, Easter, MA-Nonseasonal-01, MA-Seasonal-12

# Example 2
m <- seas(AirPassengers, x11 = "",
          regression.variables = c("td", "ao1951.jun", "ls1953.jun", "easter[14]"),
          arima.model = c(0, 1, 1, 0, 1, 1),
          check.print = c("all", "-pacf", "-pacfplot"),
          check.maxlag = 36)

m
out(m)

# 7.4 COMPOSITE 

# Remarks: The composite spec is not supported


# 7.5 ESTIMATE

# Example 1
m <- seas(AirPassengers,
          x11 = "",
          regression.variables = c("seasonal"),
          arima.model = c(0, 1, 1),
          arima.ma = "0.25f")
m
# Jan-Nov, Weekday, Easter, MA-Nonseasonal-01 = 0.25 (as specified),
# f is a float.

resid(m)
residplot(m)
# residuals of regARIMA

# Example 2
m <- seas(AirPassengers,
          x11 = "",
          outlier = NULL,
          transform.function = "log",
          regression.variables = c("td", "ao1959.01"),
          arima.model = c(1, 1, 0, 0, 1, 1),
          regression.aictest = NULL,
          estimate.tol = 1e-4,
          estimate.maxiter = 100,
          estimate.exact = "ma")
m
# Mon-Sat, AR-Nonseasonal-01, MA-Seasonal-12
# Remarks: regression.aictest has to be turned off for fully manual variable
# specification

# Example 3
seas(AirPassengers,
     x11 = "",
     regression.variables = c("td", "ao1959.01"),
     estimate.maxiter = 100,
     estimate.exact = "ma",
     arima.model = "(1 1 0)(0 1 1)",
     regression.aictest = NULL,
     outlier = NULL,
     transform.function = "log",
     regression.b = c("-0.6f", "-0.3f", "-0.3f", "-0.3f", "0.3f", "0.4f", "0.4f"),
     arima.ma = "0.57462f",
     arima.ar = "-0.22557f")
# Mon-Sat have coefficients -0.6, -0.3, -0.3, -0.3, 0.3, 0.4, 0.4
# AR-Nonseasonal-01 = -0.2256,
# MA-Seasonal-12 = 0.5746

# Remarks: Instead of saving the .mdl file as an input, users should directly
# save the call to seas().

# This can be done with the help of the static() function.

# 7.6 FORCE 
# Example 1
seas(AirPassengers,
     pickmdl = "",
     x11.seasonalma = "S3X9",
     force.start = "oct")


# Example 2
seas(AirPassengers, 
     pickmdl = "",
     x11.seasonalma = "S3X9",
     force.start = "oct",
     force.type = "regress",
     force.rho = 0.8)

# Example 3
seas(AirPassengers, 
     pickmdl = "",
     x11.seasonalma = "S3X5",
     force.type = "none")

# 7.7 FORECAST
# Example 1
m <- seas(AirPassengers,
          transform.function = "log",
          regression.variables = "td",
          arima.model = "(0 1 1)(0 1 1)12",
          forecast = "")
series(m, "forecast.forecasts")
ts.plot(series(m, "forecast.forecasts"), col = "black")

# Example 2
m <- seas(AirPassengers, 
          transform.function = "log",
          regression.variables = "td",
          arima.model = "(0 1 1)(0 1 1)12",
          forecast.maxlead = 24)

series(m, "forecast.forecasts")
# Remarks: The estimate and outlier spec are activated by default.

# Example 3
m <- seas(AirPassengers, 
          x11 = "",
          transform.function = "log",
          regression.variables = "td",
          arima.model = "(0 1 1)(0 1 1)12",
          forecast.maxlead = 15,
          forecast.probability = 0.9,
          forecast.exclude =)
series(m, "forecast.forecasts")
ts.plot(series(m, "forecast.forecasts"), col = "black")

remotes::install_github("christophsax/seasonalbook")

# Example

# CRA-Class ----

bookdata <- import.ts(file = "data/retail/Book stores.dat", format = "datevalue")

plot(bookdata)

m <- seas(bookdata, x11 = "")

out(m)

view(m)

sa <- final(m)
plot(sa)

tr <- trend(m)

plot(bookdata)
lines(sa, col = 2)
lines(tr, col = 3)
# Note: The trend has other things excluded, such as calendar effects,
# outliers, etc!

summary(m)
udg(m)

udg(m, "nfcst")

vignette("seas")


