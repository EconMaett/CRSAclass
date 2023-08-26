# Extensive guide to seasonal ----

if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

# The package seasonal imports the package x13binary and seasonalview.

# You can check out the vignette:
vignette("seas")


# seasonal depends on the x13binary package, which downloads and
# installs the X-13 binaries.
# You do not need to separately install or load X13binary.

m <- seas(AirPassengers, x11 = "")
m

head(as.data.frame(m))
# The S3 method for class 'summary.seas', as.data.frame.seas(),
# coerces the output to a data frame with columns
# date final seasonal seasonaladj trend irregular adjustfac

# The resulting data frame follows the naming conventions from the
# "broom" package, but do not depend on that package.

# You can do the same with summary(m):
as.data.frame(summary(m))
# data frame with columns
# term, estimate, std.error, statistic, p.value
# for the regressors Weekday, Easter[1],
# Ao1951.May, MA-Nonseasonal-01, MA-Seasonal-12


# checkX13 checks the installation of X-13ARIMA-SEATS.
checkX13(fail = FALSE, fullcheck = TRUE, htmlcheck = TRUE)
checkX13()

old.path <- Sys.getenv("X13_PATH")
Sys.setenv(X13_PATH = "") # it is broken now
checkX13()

Sys.setenv(X13_PATH = old.path) # fix it
checkX13()


# Use data(seasonal) to get example data
data(seasonal)
# The example data are time series objects of class "ts" including

cpi
# cpi: Monthly consumer price index of Switzerland from the Federal Statistical Office.
# Base year is 1993.

imp
exp
# Monthly exports and imports of China from July 1983 to December 2013
# in 100 mio. U.S. Dollar form China Customs.

iip
# Industrial production of India, Index value, compiled by using data from 16
# source agencies.

unemp
# United States unemployment level in thousands of persons
# from the U.S. Bureau of Labor Statistics, retrieved from FRED,
# the Federal Reserve Bank of St. Louis.

# Dates for Easter day, Diwali, and Chinese New Year, suitable for genhol().
# Objects of class "Date"
easter
diwali
cny
# They are downlaoded from the U.S. Census Bureau or the website
# http://www.chinesenewyears.info/chinese-new-year-calendar.php


# Use final() to extract the adjusted time series 
final(m)

original(m)

trend(m)

irregular(m)

head(as.data.frame(m))
# The adjustment-factors are included!

# If na.action = na.exclude was specified in the call to seas(),
# the time series in m will contain NAs.

# NA handling
end(AirPassengers) # 1960 12
# We extend the airline passengers data with NAs into the future:
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
tail(AirPassengersNA)

final(seas(AirPassengersNA, na.action = na.omit)) # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
final(seas(AirPassengersNA, na.action = na.x13)) # NA filled by x13
final(seas(AirPassengersNA, na.action = na.fail)) # fails


# Use fivebestmdl() to return the five best models as chosen by the BIC criterion.
# This works only if the automdl spec was activated, which it is by default.

# If it is not activated, the function tries to re-evaluate the model with the
# automdl spec activated.

m <- seas(AirPassengers, x11 = "")
fivebestmdl(m)
# arima          bic
# (0 1 0)(0 1 1) -4.007
# (1 1 1)(0 1 1) -3.986
# (0 1 1)(0 1 1) -3.979
# (1 1 0)(0 1 1) -3.977
# (0 1 2)(0 1 1) -3.970

# Use genhol() to generate user-defined holiday regressor variables.
# This is a replacement written in R, the U.S. Census Bureau software is not needed.

# The start argument shifts the start point of the holidays.
# The end argument shifts the end holiday.
# The center argument can be "calendar", "mean" or "none"
# Use "calendar" for Easter or Chinese New Year, which can only be in
# two months, Easter in April or March, CNY in January or February.

# Use "mean" for Ramadan, which moves through every month of the year over time.

# The resulting series can be used as regressors in seas().
# To remove the holiday effect from the final series, specify
# regression.usertype = "holiday".
# The default is to include user defined variables in the final series.

# Call data(holiday) to get teh dates of CNY, Diwali, and Easter
data(holiday)
cny
diwali
easter

### use of genhol() 

# 10 days vefore Easter day to one day after, quarterly data:
genhol(easter, start = -10, end = 1, frequency = 4)
# Easter is always in the first half-year (Q1 or Q2), so use:
genhol(easter, frequency = 2)

# centering for overall mean:
genhol(easter, center = "mean")

# centering for monthly calendar means:
genhol(easter, center = "calendar")

### replicating X-13's built-in Easter adjustment

# built-in
m1 <- seas(AirPassengers,
           regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
           arima.model = "(0 1 1)(0 1 1)",
           regression.aictest = NULL,
           outlier = NULL,
           transform.function = "log",
           x11 = ""
           )

summary(m1)
as.data.frame(summary(m1))

# user defined variable
ea1 <- genhol(easter, start = -1, end = 1, center = "calendar")

# regression.usertype = "holiday" ensures that the effect is removed
# from the final series:
m2 <- seas(AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea1,
           regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)",
           regression.aictest = NULL,
           outlier = NULL,
           transform.function = "log",
           x11 = ""
           )

as.data.frame(summary(m2))

all.equal(final(m1), final(m2), tolerance = 1e-3)
# TRUE

# Wit genhol() we can do slightly better by adjusting the length
# of easter from Friday to Monday:
ea2 <- genhol(easter, start = -2, end = 1, center = "calendar")
m3 <- seas(AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea2,
           regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)",
           regression.aictest = NULL,
           outlier = NULL,
           transform.function = "log",
           x11 = ""
           )

data.frame(summary(m3))


### Chinese New Year

# According to de facto holiday length: http://en.wikipedia.org/wiki/Chinese_New_Year,
# the de facto length is six days.
cny.ts <- genhol(cny, start = 0, end = 6, center = "calendar")

m1 <- seas(imp, 
           xreg = cny.ts,
           regression.usertype = "holiday",
           x11 = "",
           regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
           arima.model = "(0 1 2)(0 1 1)",
           regression.aictest = NULL,
           outlier = NULL,
           transform.function = "log"
           )

as.data.frame(summary(m1))

# Create an identical no-CNY model:
m2 <- seas(imp, 
           x11 = "",
           regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
           arima.model = "(0 1 2)(0 1 1)",
           regression.aictest = NULL,
           outlier = NULL,
           transform.function = "log"
           )

ts.plot(final(m1), final(m2), col = c("red", "black"))


# Modeling complex holiday effects in Chinese imports:
# - positive pre-CNY effect
# - negative post-CNY effect
pre_cny  <- genhol(cny, start = -6, end = -1, frequency = 12, center = "calendar")
post_cny <- genhol(cny, start = 0, end = 6, frequency = 12, center = "calendar")
m3 <- seas(imp,
           x11 = "",
           xreg = cbind(pre_cny, post_cny),
           regression.usertype = "holiday"
           )

as.data.frame(summary(m3))


### Adjusting Indian industrial production for Diwali
m4 <- seas(iip,
           x11 = "",
           xreg = genhol(diwali, start = 0, end = 0, center = "calendar"),
           regression.usertype = "holiday"
           )

as.data.frame(summary(m4))

# Note that without the specification of regression.usertype = "holiday",
# the Diwali effects are added back to the final series!
m5 <- seas(iip,
           x11 = "",
           xreg = genhol(diwali, start = 0, end = 0, center = "calendar"))

ts.plot(final(m4), final(m5), col = c("red", "black"))

# plot the Diwali factor in Indian industrial production
plot(series(m4, "regression.holiday"))


### Using genhol() to replicate the regARIMA estimation in R

# easter regressor
ea <- genhol(easter, start = -1, end = -1, center = "calendar")
ea <- window(ea, start = start(AirPassengers), end = end(AirPassengers))

# estimating ARIMA model in base R
arima(log(AirPassengers), 
      order = c(0, 1, 1), 
      seasonal = c(0, 1, 1),
      xreg = ea
      )

# In X-13
summary(seas(AirPassengers, 
             regression.variables = c("easter[1]"),
             regression.aictest = NULL))
# Note that X-13 defines the MA-parameters as positive 
# while base R defines them as negative.

# Select or deselect outliers by point and click.
# To quit and return the call, press ESC.
# Click several times to loop through different outlier types.

m <- seas(AirPassengers, x11 = "")
identify(m, type = c("ao", "tc", "ls"))


# Use import.spc() to import X-13 spc files.
# It generates a list of calls to seas() and import.ts() that
# can be run in R.

# The print() method displays the calls in a way that can be copy-pasted
# into a script.

# Importing the original X-13 example file
import.spc(text = '
           series{
            title="International Airline Passengers Data from Box and Jenkins"
            start=1949.01
            data=(
            112 118 132 129 121 135 148 148 136 119 104 118
            115 126 141 135 125 149 170 170 158 133 114 140
            145 150 178 163 172 178 199 199 184 162 146 166
            171 180 193 181 183 218 230 242 209 191 172 194
            196 196 236 235 229 243 264 272 237 211 180 201
            204 188 235 227 234 264 302 293 259 229 203 229
            242 233 267 269 270 315 364 347 312 274 237 278
            284 277 317 313 318 374 413 405 355 306 271 306
            315 301 356 348 355 422 465 467 404 347 305 336
            340 318 362 348 363 435 491 505 404 359 310 337
            360 342 406 396 420 472 548 559 463 407 362 405
            417 391 419 461 472 535 622 606 508 461 390 432)
            span=(1952.01, )
            }
            spectrum{
            savelog=peaks
            }
            transform{
            function=auto
            savelog=autotransform
            }
            regression{
            aictest=(td easter)
            savelog=aictest
            }
            automdl{
            savelog=automodel
            }
            outlier{ }
            x11{}
           ')

### reading .spc with multiple user regression and transformation series

# running a complex call and save output in a temporary directory
tdir <- tempdir()
seas(AirPassengers,
     xreg = cbind(
       a = genhol(cny, start = 1, end = 4, center = "calendar"),
       b = genhol(cny, start = -3, end = 0, center = "calendar")
     ),
     xtrans = cbind(sqrt(AirPassengers), AirPassengers^3),
     transform.function = "log",
     transform.type = "temporary",
     regression.aictest = "td",
     regression.usertype = "holiday",
     dir = tdir,
     out = TRUE
     )

# importing the .spc file from the temporary location
ll <- import.spc(file = file.path(tdir, "iofile.spc"))

# ll is a list containing four calls:
# - ll$x, ll$xreg, and ll$xtrans: calls to import.ts(), which
#   read the series from the X-13 data files
# - ll$seas: a call to seas() which performs the seasonal adjustment in R
str(ll)
ll$x
ll$xtrans
ll$xreg
ll$seas

# to replicate the original X-13 operation, run all four calls in a series.
# You can either copy/paste and run the print() output:
ll
## import input series
x      <- import.ts("C:/Users/matth/AppData/Local/Temp/RtmpSMWPNG/x136d48112d57a2/iofile.dta")
xtrans <- import.ts("C:/Users/matth/AppData/Local/Temp/RtmpSMWPNG/x136d48112d57a2/iofile_xtrans.dta")
xreg   <- import.ts("C:/Users/matth/AppData/Local/Temp/RtmpSMWPNG/x136d48112d57a2/iofile_xreg.dta")

## main call to 'seas'
seas(x = x, xreg = xreg, xtrans = xtrans, transform.function = "log", 
     transform.type = "temporary", regression.aictest = "td", 
     regression.usertype = "holiday")

# or use eval() to evaluate the calls.
# To evaluate the first call and import the x variable:
eval(ll$x)

# to run all foru calls in ll, use lapply() and eval():
ee <- lapply(X = ll, FUN = eval, envir = globalenv())
ee$seas # the "seas" object was produced by the final call to seas()


# Use import.ts() to import time series from X-13 data files.
# A call to ts.import() is constructed and included in the output of import.spc().

# The format argument is any valid X-13 file format as described in chapter
# 7.15 of the X-13 manual:
# - "datevalue"
# - "datevaluecomma"
# - "free"
# - "freecomma"
# - "x13save"
# - "tramo"
# - an X-11 or Fortran format.

# Save the output files to a temporary directory:
tdir <- tempdir()
seas(AirPassengers, dir = tdir)

# Import the output files:
import.ts(file = file.path(tdir, "data.dta"))
import.ts(file = file.path(tdir, "iofile.rsd"), format = "x13save")


# Use na.x13() to handle missing values with X-13.
# It substitutes NA values by -99999.

AirPassengersNA <- AirPassengers
AirPassengersNA[20] <- NA # Observation no. 20 is NA now
na.x13(AirPassengersNA)
AirPassengersNA[20] # Still NA?

seas(AirPassengersNA, na.action = na.x13)


# Use out() to see the full content of the X-13ARIAM-SEATS HTML output
# out(x, browser = getOption("browser"), ...)
getOption("browser")

# To keep the size of the "seas" object small, seas() does not save
# all output by default and instad re-evaluates the model.

m <- seas(AirPassengers, x11 = "")
out(m)

# customize the output with additional elements:
out(m, automdl.print = "autochoicemdl")


# Return the names of the outliers with outlier(x, full = FALSE).
# if full = TRUE, the full labels are shown.
m <- seas(AirPassengers, x11 = "")
outlier(m)


# Use the S3 method plot() for objects of class "seas", plot.seas():
plot(m, outliers = TRUE, trend = TRUE,
     main = "Original & adjusted series",
     xlab = "Time", ylab = "",
     transform = c("none"))

plot(m, outliers = FALSE, trend = FALSE,
     main = "YoY change (in %)", 
     xlab = "", ylab = "",
     transform = "PCY")

# You can also call residplot() and monthplot()
residplot(m, outliers = FALSE)

monthplot(m, choice = "seasonal")

# Use standard R functions
acf(resid(m))
pacf(resid(m))
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))


# Use predict.seas() to return the adjusted series.
# Equivalent to call to final().

# You can add newdata to re-evaluate the model with new data.
# This is equivalent to calling
# final(update(m, x = newdata))

# Using data from Dec 59 to estimate a model:
ap.short <- window(AirPassengers, end = c(1959, 12))
m <- seas(ap.short, x11 = "")
predict(m)
final(m) # equivalent

# Use Dec 59 model specification to estimate data up to Dec 60:
predict(m, newdata = AirPassengers)
final(update(m, x = AirPassengers)) # equivalent


# seas() is the main function of the seasonal package.
# x is an object of class "ts" or "mts"

# xreg can be holiday regressors, outliers, and level shifts.

# xtrans can include one or two transformed input series.

# seats.noadmiss = "yes" (default): If SEATS decomposition is invalid,
# an alternative model is used. If noadmiss = "no", no approxiamtion is done.
# If seats.noadmiss = NULL, no adjustment is performed.


# transform.function = "auto" (default) Automatic log-tranformation detection
# by X-13. Else use "log" or "none"

# regression.aictest = c("td", "easter") (default).
# Set to NULL to turn it off.

# outlier: spec outlier without arguments (dfault).
# Set to NULL to turn it off.

# automdl: automdl spec without arguments (default).
# Set to NULL to turn it off.

# na.action: A function to treat missing values.
# na.action = na.omit (default).
# Alternatives are na.exclude, na.fail, na.x13.
# na.x13 replaces NA with -99999.

# out: logical. Should X-13ARIMA-SEATS standard output be
# saved in the "seas" object?
# If out = TRUE, the size of the "seas" object increases substantially.

# dir: user defined file path. If specified, the X-13ARIMA-SEATS output
# files are copied to this folder.

# ... : additional spec-argument options sent to X-13ARIMA-SEATS.
# list = list(): A named list with additional spec-argument options.
# An alternative to the ... argument.

# Details:
# Almost all X-13ARIMA-SEATS options can be specified via the ... argument
# in seas().

# Such spec-argument pairs are separated by a dot (.), like
# regression.usertype = "holiday".
# Alternatively, one can use the list = list() argument like
# seas(AirPassengers, list = list(regression = c(usertype = "holiday")))

# Similarly, the series(m, "spec.argument") function can be used
# to read almost all output form X-13ARIMA-SEATS.
# The udg() function provides access to the diagnostic statistics.


# Return values:

# series
# A list containing the output tables of X-13 that are saved
# and can be directly read with series(). It is much faster than
# re-evaluating the model.
m <- seas(AirPassengers, x11 = "")
names(m$series) # "d10" "d11" "d12" "d13" "d16" "e18" "rsd"
m$series$rsd

# data
# The adjusted data, the raw data, the trend component, the irregular component
# and the seasonal component (deprecated)
head(m$data)

# err
# Waring messages from X-13ARIMA-SEATS
m$err
names(m$err) # "error"   "warning" "note" 
m$err$error
m$err$warning
m$err$note

# udg
# Content of the .udg file
m$udg
udg(m)

# est
# content of the .est output file
m$est
names(m$est) # reg, arima, variance, coefficients, se
m$est$reg
m$est$arima
m$est$variance
m$est$coefficients
m$est$se

# model
# list with model specification, similar to the .spc file.
# typically contains "regression" with he regressors and parameter estimates
# and "arima" with the ARIMA specification and parameter estimates.
m$model
m$model$regression
m$model$regression$variables
m$model$regression$b

m$model$arima
m$model$arima$model
m$model$arima$ma

# fivebestmdl
# Best five ARIMA models (unparsed)
m$fivebestmdl

# x
# input series
m$x

# spc
# object of class "spclist", a list containing the content of the 
# .spc file used by X-13ARIMA-SEATS.
# Each spec is on the first level, each argument on the secondlevel
m$spc
m$spc$series
m$spc$x11
m$spc$transform

# call
# function call
m$call

# wdir
# temporary directory in which X-13ARIMA-SEATS has been run
m$wdir

# Examples for seas():
m <- seas(AirPassengers, x11 = "")
summary(m)
as.data.frame(summary(m))

# Graphical userface
view(m)

# Invoke the X-13ARIMA-SEATS options as spec.argument pairs through
# the ... argument:
seas(AirPassengers, regression.aictest = c("td")) # no easter testing

seas(AirPassengers, force.type = "denton") # force equality of annual values

seas(AirPassengers, x11 = "") # override seats spec

# spec.argument combinations can also be supplied to a named list:
seas(AirPassengers, 
     list = list(
       regression.aictest = c("td"),
       outlier = NULL
     ))

# This is useful for programming. For example, we want to produce the list
# step by step:
ll <- list() # start with an empty list
ll[["x"]] <- AirPassengers # supply the input data
ll[["regression.aictest"]] <- "td" # no easter testing
ll[["outlier"]] <- list(NULL) # assigning NULL to a list using single brackets

# call seas()
seas(list = ll)

# The options can also be entered as vectors:
seas(AirPassengers, regression.variables = c("td1coef", "easter[1]"))

seas(AirPassengers, arima.model = c(0, 1, 1, 0, 1, 1))
seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)") # equivalent

# To turn off the automatic procedures, set them equal to NULL
seas(AirPassengers, 
     regression.aictest = NULL,
     outlier = NULL,
     transform.function = "log",
     arima.model = "(0 1 1)(0 1 1)",
     regression.variables = c("td1coef", "easter[1]")
     )

# static replication of m <- seas(AirPassengers):
static(m) # This also tests the equivalence of the static call
static(m, test = FALSE) # No testing (faster)
static(m, coef = TRUE) # fixes the coefficients
# regression.b = c("-0.002949699141f", "0.01776737357f", "0.1001558244f"),
# arima.ma = c("0.1156204139f", "0.4973600193f")
# the f means the coefficients are fixed, else they are starting values.

# updating an existign model
update(m, x11 = "")

# specific extractor functions:
final(m)
predict(m) # equivalent
original(m)
resid(m)
coef(m) # coefficients
fivebestmdl(m) 
out(m)
spc(m) # the .spc input file

# universal extractor function for any X-13ARIMA-SEATS output,
# see
?series
series(m, "forecast.forecasts")

# copying the output of X-13 to a user defined directory
seas(AirPassengers, dir = tempdir())

# forecasting an annual series without seasonal adjustment
m <- seas(airmiles, seats = NULL, regression.aictest = NULL)
series(m, "forecast.forecasts")

# NA handling
AirPassengersNA <- window(AirPassengers, end = end(AirPassengers)+1, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
final(seas(AirPassengersNA, na.action = na.fail)) # fails

# NA ahndling by X-13 (works with internal NAs)
AirPassengersNA[20] <- NA
final(seas(AirPassengersNA, na.action = na.x13))

## performing composite adjustment
m.direct <- seas(ldeaths, x11 = "")
final.direct <- final(m.direct)

m.indirect <- lapply(X = list(mdeaths, fdeaths), FUN = seas, x11 = "")
# the additional argument x11 = "" was passed to lapply(X, FUN, ...)

# not very efficient, but keeps time series properties
final.indirect <- Reduce(f = `+`, x = lapply(X = m.indirect, FUN = final))

ts.plot(cbind(final.indirect, final(m.direct)), col = 1:2)
legend("topright", legend = c("disaggregated", "aggregated"), lty = 1, col = 1:2)


# series() with the exception of the composite spec, series() imports all tables
# that can be saved in X-13ARIMA-SEATS
# reeval = TRUE (default)
# verbose = TRUE (default): message returned if a spec is added 
# during reevaluation.

# Details:
# If the save argument is not specified in the model call to seas(),
# series() re-evaluates the call with the corresponding specs enabled
# and returning a message.
# This doubles computation time.

# Examples:
m <- seas(AirPassengers)
series(m, "fct") # re-evaluates the model with the forecast spec activated and saved

# You can specify multiple series
series(m, c("rsd", "fct"))

m <- seas(AirPassengers, forecast.save = "fct")
series(m, "fct") # much faster because no re-evaluation needed

# fct is short for forecast.forecasts.

# history psec
series(m, c("history.trendestimates", "history.sfestimates", "history.saestimates"))

# slidingspans spec
series(m, c("slidingspans.sfspans", "slidingspans.tdspans"))

# fundamental identities of seasonal adjustment
# Y = T * I * (S * TD)
all.equal(
  target = AirPassengers, 
  current = series(m, "seats.trend") * series(m, "seats.irregular") * series(m, "seats.adjustfac")
  )
# TRUE
# Y_sa = Y / (S * TD)
all.equal(
  target = final(m), 
  current = AirPassengers / series(m, "seats.adjustfac")
    )
# TRUE
# Note: the series seats.adjustfac includes the adjustment for
# both the seasonal component and the trading day component!


### Some X-13ARIMA-SEATS functions can be replicated in R:

# X-13ARIMA-SEATS spectrum
plot(series(m, "spectrum.specorig")[, -1], t = "l")

# R equivalent: spectrum from stats package
spectrum(diff(log(AirPassengers)), method = "ar")


# X-13 ARIMA-SEATS pacf
x13.pacf <- series(m, "identify.pacf") # Sample_PACF, S.E._of_PACF
plot(x13.pacf[, 1], t = "h")
lines(x13.pacf[, 2])
lines(-x13.pacf[, 2])
graphics.off()

# R equivalent: pacf from stats package
pacf(AirPassengers, lag.max = 35)


# Use the spc() function to access the .spc file content
m <- seas(AirPassengers)
spc(m)


# SPECS prvides a list of available X-13ARIMA-SEATS outputs
# The data is used by several functions as a look-up table.

# Users should consider the table in series() or in the official manual.
# It is a data frame
names(SPECS) # long, short, spec, is.save, is.series, requires

head(SPECS)


# static()
# In a static call, the default automatic procedures in the model call are
# substituted by the choices they made.

# if coef = TRUE, the coefficients are treated as fixed instead of being
# re-estimated.
# if x11.filter = TRUE, the X-11 moving averages will be fixed.
# If test = TRUE, The new static call is executed and compared to the previous call.
# If the final series are different, a message is returned.

# If fail = TRUE, differences in the final series will cause an error.
# Ignored if test = FALSE.

# If evaluate = TRUE, the new call is evaluated.

# The evaluated call can be copy/pasted into a script.

# Use the getCall() funciton from the stats package:
getCall(m) # default call

static(m) # REturns all the options that were defined (implicitly).
# seas(
#   x = AirPassengers,
#   regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#   arima.model = "(0 1 1)(0 1 1)",
#   regression.aictest = NULL,
#   outlier = NULL,
#   transform.function = "log"
# )

static(m, test = FALSE) # Much faster, because previous call is not re-evaluated
static(m, evaluate = TRUE) # returns the "seas" object returned by the call

m <- seas(AirPassengers, x11 = "")
static(m, x11.filter = TRUE) # fix the X-11 filter (with warning)
# Static series is different. Mean relative difference: 0.001944792
# seas(
#   x = AirPassengers,
#   x11 = "",
#   regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#   arima.model = "(0 1 1)(0 1 1)",
#   regression.aictest = NULL,
#   outlier = NULL,
#   transform.function = "log",
#   x11.trendma = 9,
#   x11.seasonalma = "s3x3"
# )

static(m, coef = TRUE) # fixes the coefficients
# seas(
#   x = AirPassengers,
#   x11 = "",
#   regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#   arima.model = "(0 1 1)(0 1 1)",
#   regression.aictest = NULL,
#   outlier = NULL,
#   transform.function = "log",
#   regression.b = c("-0.002949699141f", "0.01776737357f", "0.1001558244f"),
#   arima.ma = c("0.1156204139f", "0.4973600193f")
# )


# summary.seas() returns the summary for the regARIMA model.

# sumamry.seas() is the S3 method for objects of type "seas"
# summary(object, stats = getOption("seas.stats"), ...)

# print.summary.seas() is the S3 method for objects of type "summary.seas"
# print(x, digits = max(3, getOptions("digits") - 3), 
# signif.stars = getOption("show.signif.stars"), ...)

# x is an object of class "summary.seas", the result of a call to
# summary.seas().

# digits: number of singificant digits printed
# signif.stars: If significance stars should be printed for each coefficient.

# Details:
# Adjustment: SEATS or X-11
# ARIMA: ARIMA(p d q)(P D Q)
# Obs. number of observations
# Transform: prior transformation: log or none
# AICc, BIC, value of information criterion (the lower the better)
# QS: test for seasonality in final series.
# Null hypothersis: No seasonality in final series.
# Significance codes are shown if the null hypothesis is rejected.
# QS statistics for more series can be extracted with qs()
qs(m)


# Box-Ljung test for residual autocorrelation.
# Null hyptohesis: No autocorrelation in residuals.
Box.test(resid(m), lag = 24, type = "Ljung")

# Shapiro: Test for normailty of residuals:
# Null hypothesis: Normal distrubution of residuals
shapiro.test(resid(m))
plot(acf(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))

# summary.seas() returns a list with the summary statistics included in 
# object, and the computations of the following additional statistics:

# coefficients: matrix with coefficients, standard errors, and p-values
# transform: transformation
summary(m)
as.data.frame(summary(m))

sum.m <- summary(m)
sum.m$coefficients # Weekday, Easter, AO, MA-01, MA-Seasonal-12
sum.m$transform.function # "log"

# Change the options to include the M quality statistics for X11 in summary:
m <- seas(AirPassengers)
summary(m)

options(seas.stats = c("f3.m01", "f3.m02", "f3.m03", "f3.mo4"))
summary(seas(AirPassengers, x11 = ""))
# f3.m01: 0.041  f3.m02: 0.042  f3.m03: 0 

# Note that this has not affected the SEATS outptut:
summary(seas(AirPassengers))


# reset to default options:
options(seas.stats = NULL)


# Get the transformation that was applied with transformfunction():
transformfunction(m) # "log"

# Use the udg() function to get diagnostical statistics.
# For quick access, use the AIC(), BIC(), and logLik() functions
# which are wrappers that use udg() to access the statistics
udg(m)

udg(m, "peaks.tukey.p90.td")

qs(m)

AIC(m)
BIC(m)
nobs(m)
logLik(m)


# extract multiple stats from udg():
m <- seas(AirPassengers, x11 = "")
udg(m, c("f3.m02", "f3.m05", "qsori")) # returns a list
udg(m, c("f3.m02", "f3.m05")) # returns a named vector

# faster than:
udg(m)[c("f3.m01", "f3.m02", "qsori")]


# update.seas()
# update and re-evaluate a model

m <- seas(AirPassengers)
update(m, x11 = "")
update(m, x = sqrt(AirPassengers), x11 = "")

# Use update() with lapply() or mapply():
dta <- list(fdeaths = fdeaths, mdeaths = mdeaths)

# use seas() in lapply()
ll <- lapply(X = dta, FUN = seas, x11 = "")

# Use update() in lapply()
lapply(X = ll, FUN = update, arima.model = c(0, 1, 1, 0, 1, 1))


# The view() function has a story argument that is the local file path
# or URL to an .Rmd file

# If quiet = TRUE, error messages from calls in view() are not displayed in the
# console.

# The ... argument passes additional arguments to the runApp() function
# from the shiny package.
# E.g., for selecting if the GUI should open in the browser or in the
# RStudio viewer pane.

# You can store the modl after closing the GUI
m <- seas(AirPassengers)
view(m)

m.upd <- view(m)

