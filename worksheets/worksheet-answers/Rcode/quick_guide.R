# R seasonal package quick guide ----

## install and load the seasonal package ----
if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

# This downloads the seasonal package, the seasonalview package,
# and the X-13ARIMA-SEATS executable.

# Time series data ----
# The seasonal package uses time series of class "ts" as input.
# Use the import.ts() function to read text files that X-13ARIMA-SEATS can
# read into R:
myData <- import.ts(file = "data/retail/Automobile and other motor vehicle dealers.dat", format = "datevalue")
?import.ts # A function from seasonal

# format: A valid X-13 file format
# "datevalue", "datevaluecomma", "free",
# "freecomma", "x13save", "tramo", or
# an X-11 or Fortran format

# Examples:
tdir <- tempdir()
seas(AirPassengers, dir = tdir)
import.ts(file = file.path(tdir, "iofile.dta"))
import.ts(file = file.path(tdir, "iofile.rsd"), format = "x13save")
rm(tdir)

# Running X-13ARIMA-SEATS with seas() ----
m <- seas(AirPassengers)
# Invisibly, in the background, a spec file is written,
# X-13ARIMA-SEATS is run with that spec file,
# the output is read into R,
# an object of class "seas" named "m" is created.

# The default settings will:
# - Test for the transformation (log vs none)
# - Test for (flow) trading day effects
# - Test for Easter moving holiday effects
# - Look for additive outliers (AO) and level shifts (LS)
# - Forecast one year of data (X11) or three years (SEATS)
# - Adjust the series with X11 or SEATS (default)

# To change the adjustment settings, add for example:
# - arima.model = "(0 1 1)(0 1 1)" to specify an airline model
# - regression.variables = c("AO2008.Jul", "td1coef", "Easter[1]") to set regression variables
# - forecast.maxlead = 60 to set teh number of forecasts to 60
# - series.modelspan = "2005.1" to set the model span to start in 2005.1
# - x11 = "" to run the default X-11 adjustment instead of SEATS

## Accessing output ----

# Open the HTML output file for the run
out(m)

# Print a summary of the regression results and diagnostic information
# to the console window
summary(m)

# Print regression and ARIMA estimates and standard errors
m$est

final(m)     # Extract the adjusted series
original(m)  # Extract the original series
trend(m)     # Extract the trend
irregular(m) # Extract the irregular component
residuals(m) # Extract the residuals


# Extract a named table:
series(m, "forecast.forecasts")

# If the requested table has not already been listed
# in a save argument when running seas(), seas() will be re-run
# to create the requested table.

# Tables have a long name like "spec.table" and a short abbreviation.
# Appendix B of the X-13 manual lists the abbreviations.

# Examples:
series(m, "forecast.forecasts") # Save the forecasts
series(m, "fct")                # Same result

series(m, "x11.irrwt") # Save the final irregular weights
series(m, "c17")       # Same result

# Get the diagnostics information with the udg() function
?udg
udg(m)
# Use udg(m, "udg key name") to extract information

static(m)
# Creates a call (itself a seas object) of m with auto-choices hard-coded.

# Include x11.filter = TRUE to hard-code the x-11 filter.
m.static <- static(m, x11.filter = TRUE)

# Evaluate the call with eval(static seas object)
eval(expr = m.static)


## Graphing output ----
plot(m, outliers = TRUE, trend = TRUE, 
     main = "Original series, seasonal adjustment and trend",
     xlab = "", ylab = "")

plot(m, outliers = TRUE, trend = TRUE,
     main = "Month to month percent change of original series, seasonal adjusmten and trend",
     xlab = "", ylab = "",
     transform = "PC")

plot(m, outliers = TRUE, trend = TRUE,
     main = "Year on year percent change of original series, seasonal adjusmten and trend",
     xlab = "", ylab = "",
     transform = "PCY")

monthplot(m, choice = "seasonal",
          main = "Seasonal factors and SI ratios")

monthplot(m, choice = "irregular",
          main = "Irregular component")

residplot(m, outliers = TRUE, main = "regARIMA residuals")
abline(a = 0, b = 0)
graphics.off()

acf(resid(m), main = "ACF of residuals")
pacf(resid(m), main = "PACF of residuals")
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))
# Access the help function fo the method plot()
# for objects of class "seas":
?plot.seas
# END