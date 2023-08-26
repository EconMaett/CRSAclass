# Exercise 4 - genhol ----

## Prep ----
if (!require(seasonal)) { install.packages("seasonal") }
library(seasonal)

data(seasonal)
data(holiday) # includes the vector "easter" of type "Date"
head(easter) # dates of Easter Day from 1931 to 2030

## Part 1 ----
# Create an Easter Monday regressor called "emon" for a monthly
# series (frequency = 12).

# The holiday runs from one day after to one day after Easter,
# and is calendar-centered (because Easter occurs only in one of the
# same two months every year)

# Note that "easter" includes the dates of Easter Sundays, not Easter Mondays
# By defining start = -1 and end = -1, we only get the dates of Easter Monday.
eamon <- genhol(easter, 
                start = -1, 
                end   = -1, 
                frequency = 12, 
                center = "calendar"
                )


# Import the IGAE tertiary sector data
f <- "data/mx/Services output (IGAE tertiary sector).dat"
ts.ser <- import.ts(file = f, format = "datevalue")

plot(ts.ser)
graphics.off()

# Easter[1] included by default
# LS1995.Feb, LS1995.Apr, Ao1996.Feb, Ao2008.Feb, LS2008.Dec, TC2009.Apr
# Transform: log
# ARIMA: (1 1 0)(0 1 1) 

# Run X-13:
ser.m1 <- seas(ts.ser,
               transform.function = "log",
               arima.model = "(1 1 0)(0 1 1)",
               regression.aictest = NULL, 
               regression.variables = c(
                 "tdnolpyear", "lpyear", "easter[3]",
                 "LS1995.Feb" , "LS1995.Apr", "LS2000.Jan", 
                 "LS2008.Dec", "TC2009.Apr"),
               outlier.types = "all"
               )

summary(ser.m1)
summary(ser.m1)$aicc # 573.0578
# Easter[3] is highly significant.

# Add the commands to include the eamon regressor.
# Run the new seasonal object as ser.m2

# Note: Easter Monday is not commonly celebrated in the U.S.,
# hence this is a typical case for genhol.
ser.m2 <- seas(ts.ser,
               transform.function = "log",
               arima.model = "(1 1 0)(0 1 1)",
               regression.aictest = NULL, 
               regression.variables = c(
                 "tdnolpyear", "lpyear", 
                 "easter[3]",
                 "LS1995.Feb" , "LS1995.Apr", "LS2000.Jan", 
                 "LS2008.Dec", "TC2009.Apr"),
               outlier.types = "all",
               xreg = eamon,
               regression.usertype = "holiday"
               )

summary(ser.m2)
summary(ser.m2)$aicc # 575.2671
# If we include both Easter[3] and xreg, both are not significant anymore.

# The regressor "xreg" is highly significant if we remove Easter[3].

# From the X-13 manual:
# easter[w]: Easter holiday regression variable for monthly or quarterly 
# flow data that assumes the level of daily activity changes 
# on the w-th day before Easter and remains at the new level through the
# day before Easter. 

# Note: This is typical for retail sales if shops are closed on Easter Sunday
# in the US, people do all their shopping in the three days before.
# This value w must be supplied and can range from 1 to 25. 

# A user can also specify an easter[0] regression variable, 
# which assumes the daily level of activity level changes only on Easter Sunday. 

# To estimate complex effects, several of these variables, differing in their
# choices of w, can be specified

# so 
# regression.variables = easter[3] 
# is equivalent to 
# xreg = genhol(easter, start = -3, end = -1, frequency = 12, center = "calendar")


# Compare the AICc of the models with and without Easter Monday.
summary(ser.m1)$aicc # 573.0578 without Easter Monday
summary(ser.m2)$aicc # 575.2671 with Easter Monday


# Change the seasonal object so the model span starts in 2008.
# Is Easter MOnday significant?

# It is not significant if we already included Easter Sunday...
ser.m3 <- seas(ts.ser,
               transform.function = "log",
               arima.model = "(1 1 0)(0 1 1)",
               regression.aictest = NULL, 
               regression.variables = c(
                 "tdnolpyear", "lpyear",
                 "easter[3]",
                 "LS1995.Feb" , "LS1995.Apr", "LS2000.Jan", 
                 "LS2008.Dec", "TC2009.Apr"
                 ),
               outlier.types = "all",
               xreg = eamon,
               regression.usertype = "holiday",
               series.modelspan = "2008.01,"
               )

summary(ser.m3)
summary(ser.m3)$aicc
# Note that now xreg is highly significant, but easter[3] was removed.