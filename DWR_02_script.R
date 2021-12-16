# Data Wrangling in R, Part 2 of 3
# Dates and Strings
# Fall 2018
# UVa Library - Research Data Services
# Clay Ford

library(tidyverse)
library(lubridate)


# Load data ---------------------------------------------------------------

# Let's first load some data we'll use today.

# Before running the following code you may want to set your working directory.
# Go to Session...Set Working Directory...Choose Directory...

# The "earthquakes.csv" file contains all Earthquake data recorded by the USGS
# in the last 30 days as of 09-Dec-2016, with one record per earthquake;
# downloaded from the following site:
# http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php

URL1 <- "http://people.virginia.edu/~jcf2d/data/earthquakes.zip"
data1 <- basename(URL1)
download.file(url = URL1, destfile = data1)
unzip(data1)
quakes <- read.csv("earthquakes.csv", stringsAsFactors = FALSE)
str(quakes)

# Notice that when we read in CSV files with dates and strings, we set
# stringsAsFactors = FALSE. This prevents dates and strings from being formatted
# as factors.

# In January 2015, the Occupational Safety and Health Administration began
# requiring U.S. employers to report "all severe work-related injuries, defined
# as an amputation, in-patient hospitalization, or loss of an eye."  The
# "severeinjury2016.csv" file contains this data for 2016, including the injury
# dates, descriptions, and outcomes, as well as the employers' names and
# locations. Source: https://www.osha.gov/severeinjury/index.html

URL2 <- "http://people.virginia.edu/~jcf2d/data/severeinjury2016.zip"
data2 <- basename(URL2)
download.file(url = URL2, destfile = data2)
unzip(data2)
injuries <- read.csv("severeinjury2016.csv", stringsAsFactors = FALSE)
str(injuries)

# Release dates, run times and box office gross of the Marvel Cinematic
# Universe.
# https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films
mcu <- read.csv("http://people.virginia.edu/~jcf2d/data/marvel_movies.csv",
               stringsAsFactors = FALSE)
str(mcu)
# Use the parse_number() function from the readr() package to format the
# box_office as numeric.
mcu$box_office <- parse_number(mcu$box_office)
mcu


# Creating date/times -----------------------------------------------------

# When working with dates, We either work with dates or date-times. 

# A date example is April 5, 1973. 
# A date-time example is 1973-04-05 12:23:12.

# To format dates, lubridate provides a series of functions that are a
# permutation of the letters "m", "d" and "y" to represent the ordering of
# month, day and year. 

# Examples:
mdy("April 5, 1973")
mdy("4-5-1973")
mdy("04/05/1973")
mdy("4/5/73")

dmy("5-April-73")
dmy("5 April 1973")

ymd("1973 4 5")
ymd("1973-04-05")

bday <- mdy("April 5, 1973")
bday

# It prints like character data, but it's actually number of days since
# 1970-01-01. (Dates prior to 1970-01-01 are negative numbers.) Use as.numeric()
# to verify.
as.numeric(bday)

# Let's fix our Marvel movie dates
head(mcu$release_date) # day-month-year, or dmy
mcu$release_date <- dmy(mcu$release_date)
str(mcu)
head(as.numeric(mcu$release_date))

# Dates can include times. Again we can use lubridate to format the date by
# including _hms or _hm after the date function.
bday2 <- ymd_hms("1973-04-05 20:11:59")
bday2

# This is stored as number of seconds since (or before) 01-Jan-1970
as.numeric(bday2)

# lubridate can handle times with AM/PM times. It will convert it to 24 hour 
# time. For example, here's a character string that says 3:11 PM. lubridate
# converts it to 15:11.
bday3 <- ymd_hm("1973-04-05 3:11 PM")
bday3

# By the way, if you wanted to display 3:11 PM, you need to use format with
# strptime codes!
format(bday3, "%Y-%m-%d %r")


# We can also read in times without dates using the functions ms, hm, or hms,
# where again "h", "m", and "s" stand for "hours", "minutes", and "seconds".
# Here are a few examples.

time1 <- c("1:13", "0:58", "1:01")
(time1 <- ms(time1))

time2 <- c("12:23:11", "09:45:31", "12:05:22")
(time2 <- hms(time2))

time3 <- c("2:14", "2:16", "3:35")
(time3 <- hm(time3))

# Once again, don't be fooled by the print out. These times are actually stored
# as seconds. Use as.numeric() to verify.
as.numeric(time1)
as.numeric(time2)
as.numeric(time3)


# How can we format the run times of the Marvel movies?
head(mcu$run_time)

# Notice hm, ms or hms does not work:
hm(mcu$run_time)
ms(mcu$run_time)
hms(mcu$run_time)


# The "min" is not parsing; we could replace " min" with ":00" and then use ms()
mcu$run_time2 <- str_replace(mcu$run_time, " min", ":00")
mcu$run_time2
mcu$run_time2 <- ms(mcu$run_time2)
mcu$run_time2
as.numeric(mcu$run_time2)

# An easier way is to use the period and duration functions.
duration(mcu$run_time)
period(mcu$run_time)


# YOUR TURN #1 ------------------------------------------------------------

# 1. Parse the EventDate column in the injuries data frame so it is stored as a
# Date (ie, number of days since 1/1/1970)



# 2. Parse the time and updated columns in the quakes data frame so they're
# stored as a Date-Time (ie, number of seconds since 1/1/1970). Hint: don't
# worry about letters in the dates. Just use what you think is the correct
# lubridate function.




# Get date components -----------------------------------------------------

# Once dates are formatted as date or date-time objects, we can pull out
# individual parts with accessor functions:

# - year()
# - month()
# - mday() (day of the month)
# - yday() (day of the year)
# - wday() (day of the week)
# - hour()
# - minute()
# - second()

(NOW <- now())  # now() returns current date-time
year(NOW)
month(NOW)
month(NOW, label = TRUE)
month(NOW, label = TRUE, abbr = FALSE)
wday(NOW)
wday(NOW, label = TRUE)
wday(NOW, label = TRUE, abbr = FALSE)
mday(NOW)
yday(NOW)
hour(NOW)
minute(NOW)
second(NOW)

# Let's add the day of the week, month, and year of the movie release as columns
# to the mcu data frame:
mcu <- mcu %>% mutate(day = wday(mcu$release_date, label = TRUE, abbr = FALSE),
                      month = month(mcu$release_date, label = TRUE, abbr = FALSE),
                      year = year(mcu$release_date))
mcu

# some quick summary stats
table(mcu$month)

# total box office by year
bo_total <- mcu %>% 
  group_by(year) %>% 
  summarize(totalBoxOffice = sum(box_office))
bo_total

# visualize total box office by year
ggplot(bo_total, aes(x = year, y = totalBoxOffice)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 2008:2018) +
  scale_y_continuous(labels = scales::dollar) +
  theme(panel.grid.minor.x = element_blank()) +
  ggtitle("Total MCU Box Office by Year") +
  labs(x = "Year", y = "Total Box Office")

# NOTE: Come to the Data Visualization in R workshop to learn more about ggplot
# on Wed, Oct 3, 2018!


# YOUR TURN #2 ------------------------------------------------------------

# 1. Add a column called "Day_of_Week" to the injuries data frame that indicates what
# day of week the injury occured on, such as "Friday" or "Saturday". 



# 2. Add a column called "Month" to the injuries data frame that indicates what
# month the injury happened in, such as "April" or "May"



# 3. CHALLENGE: Create a barplot to visualize the distributions of injuries over
# months. Hint: Use table() nested in barplot()



# Time spans --------------------------------------------------------------

# Once we have dates/times stored as number of days/seconds, we can calculate
# time spans.

# lubridate provides three classes, or three different ways, to distinguish
# between different types of time spans.

# 1. Duration
# 2. Interval
# 3. Period

# The most simple is Duration. This is simply a span of time measured in 
# seconds. There is no start date. 
 
# An Interval is also measured in seconds but has an associated start date.
# An Interval measures elapsed seconds between two specific points in time.
 
# A Period records a time span in calendar time. For example, we can add 1 month
# to February 1 and get March 1, regardless of leap years. It allows us to
# perform calculations in calendar time as opposed to number of seconds.

# Let's illustrate these three classes using two dates: 3-11-2017 and 3-12-2017.
# Notice daylight savings began on March 12 at 2:00 AM.

start <- mdy_hm("3-11-2017 5:21 PM", tz = "US/Eastern")
end <- mdy_hm("3-12-2017 5:21 PM", tz = "US/Eastern")

# Let's first create an interval. We can do that with the interval operator:
# %--%.
time.interval <- start %--% end 

# Notice how it prints
time.interval
as.numeric(time.interval)

# How long is this interval? We can answer in two ways: (1) in the precise form 
# of a duration, or (2) in less-precise the calendar/clock form. For this we can
# use as.duration and as.period.

time.interval
as.duration(time.interval)
as.period(time.interval)

# The duration returns a precise answer of 82800 seconds, or 23 hours. The 
# period returns a human-like answer of 1 day. The period perhaps makes more 
# sense since our start and end date is one day apart, but the duration is more 
# precise. It takes daylight savings in account and returns the actual amount of
# elapsed time.

# Let's revisit the run times of the marvel movies
mcu$run_time

# Notice we can specify these directly as durations
duration(mcu$run_time)

# Further, notice we can specify the duration as periods:
as.period(duration(mcu$run_time))

# And this yields a more human-friendly version of run time. Let's save to our
# data frame.
mcu$run_time2 <- as.period(duration(mcu$run_time))
mcu %>% select(movie, run_time, run_time2)



# time spans between dates in a single vector -----------------------------


# We can calculate time spans between dates in a single vector using the
# int_diff() function.
Dates <- mdy_hm(c("6/14/18 1:12 PM", "6/15/18 3:30 PM", 
                  "6/15/18 8:24 PM", "6/16/18 9:15 AM"))

# Since we have 4 dates, we'll have 3 time spans.
int_diff(Dates)
# stored as number of seconds
as.numeric(int_diff(Dates))
# convert to duration
as.duration(int_diff(Dates))


# time span conversions ---------------------------------------------------

# Durations are output precisely as seconds, but we might want to view that as 
# precise hours or weeks. We can do that with the time_length() function.

# Example 1: My son was born April 16, 2001. How old is he right now?
born <- mdy("April 16, 2001")

# first, create the interval:
life <- born %--% today() 
life               # interval

# View interval as a duration
as.duration(life) 

# View interval as a period
as.period(life)

# Age in precise minutes, hours, days, weeks and years
time_length(life, "minutes")
# as.duration(life)/dminutes(1)
time_length(life, "hours")
time_length(life, "days")
time_length(life, "weeks")
time_length(life, "years")



# Let's calculate time between marvel movie releases and convert to months.
int_diff(mcu$release_date)
time_length(int_diff(mcu$release_date), unit = "months")

# To add to the mcu data frame, we need to add a missing field to the beginning
# of the vector.
mcu$release_interval <- c(NA, time_length(int_diff(mcu$release_date), unit = "months"))
mcu %>% 
  select(movie, release_date, release_interval)

# examine time between releases
summary(mcu$release_interval)



# YOUR TURN #3 ------------------------------------------------------------


# Examine the time between incidents for Virginia companies in the injuries data
# frame. The following creates a vector of dates for incidents in Virginia.
VAdates <- injuries$EventDate[injuries$State == "VIRGINIA"]


# What is mean amount of days between events in Virginia?



# back to slides.




# character data in R -----------------------------------------------------

# Create character data by surrounding with double or single quotes; will always
# print with double quotes.
(txt1 <- " hello ")
(txt2 <- c('male','female','male'))

# Notice the double quotes below are escaped with \; also notice the new line is
# printed as \n.
(txt3 <- 'Bob "the animal" Davis
A True legend of backyard wrestling')

# Use the cat() function to see character data without escapes and \n
cat(txt3)

# Recall that all character data have quotes, but not all output with quotes is
# character data.
mdy("June 2, 2012") # not character

rm(txt1, txt2, txt3)




# Basic clean-up functions ------------------------------------------------

# We'll use functions in the stringr package for working with character data.

# Trim white space from both ends
str_trim("  word  ")

# Trim extra space throughout string
str_squish(" are    you    serious? ")

# case conversion
# lower case
head(mcu$movie)
str_to_lower(head(mcu$movie))

# upper case
str_to_upper(head(mcu$movie))

# title case
head(injuries$City)
str_to_title(head(injuries$City))

# search and replace within strings
# NOTE: n.e.c. stands for "not elsewhere classified"
head(injuries$EventTitle)
str_replace(string = head(injuries$EventTitle), ", n.e.c.", "")

# WARNING! Be careful with periods when defining patterns. Recall that stringr
# functions assume a regular expression by default. 
str_replace(string = c("Fall, n.e.c.", "Leg, knee cap"), "n.e.c.", "")

# Two ways to ensure periods are literal:

# 1) use \\ before period to "escape"
str_replace(string = c("Fall, n.e.c.", "Leg, knee cap"), ", n\\.e\\.c\\.", "")

# 2) use the fixed() function in stringr
str_replace(string = c("Fall, n.e.c.", "Leg, knee cap"), fixed(", n.e.c."), "")


# Note the difference between str_replace and str_replace_all; str_replace only
# replaces first occurrence
str_replace("434-555-1212", "-", "")
str_replace_all("434-555-1212", "-", "")

# Pad strings with characters
ids <- c(1,2,5,7,9,12,212)
# set to three digits wide, with leading 0s where necessary
str_pad(ids, width = 3, side = "left", pad = "0")



# YOUR TURN #4 ------------------------------------------------------------

# 1) Modify the Employer column in the injuries data frame so that it has Title case


# 2) The Final Narrative column contains a description of how the injury
# occurred. To make the column easier to work with, change the Final Narrative
# column to consist of all lower case letters.


# 3) Fix the zip code column in the injuries data frame so that all zip codes
# have 5 digits and a leading 0 if necessary




# character data in quakes and injuries -----------------------------------

# Notice the place column of the quakes data is character:
head(quakes$place)
is.character(quakes$place)

# Some questions we might want to answer based on information in the "place"
# column:
# - Number of earthquakes in California
# - summary of distance (mean, min, max)

# The place pattern appears to be:
# distance in km direction of city, state (or city, country)


# The injuries data frame has several columns of character data. Two of interest
# are:
head(injuries$FinalNarrative)
head(injuries$EventTitle)

# Some questions we might want to explore in the injuries data
# - How many people suffered falls?
# - How many injuries involved fingers?




# Extracting text ---------------------------------------------------------

# The str_extract function allows us to extract strings that match a pattern. We
# can use regular expressions to define patterns.

# Example 1: extract one or more numbers at the beginning of the string.
str_extract(c("12 points", "23 pts", "15"), 
            pattern = "^\\d+")
# To make numeric
as.numeric(str_extract(c("12 points", "23 pts", "15"), 
            pattern = "^\\d+"))

# Example 2: extract one or more characters at the beginning of the string
# followed by a colon.
str_extract(c("Clay: I ran.", 
              "Nancy: Where?", 
              "Clay: I ran so far away."), 
            pattern = "^\\w+(?=:)")

# Example 3: extract the name of the journal
refs <- c('A. Agresti and B.A. Coull, Approximate is better than "exact" for 
          interval estimation of binomial proportions, American Statistician, 
          52:119-126, 1998.',
          'R.G. Newcombe, Logit confidence intervals and the inverse sinh 
          transformation, American Statistician, 55:200-202, 2001.',
          'L.D. Brown, T.T. Cai and A. DasGupta, Interval estimation for a 
          binomial proportion (with discussion), Statistical Science, 
          16:101-133, 2001')
refs

# use str_squish to get rid of line breaks and excess space
str_squish(refs)


# Extract one or more letters or space that follows ', ' and is followed by ', '
# and one or more digits.
str_extract(str_squish(refs), pattern = "(?<=, )[a-zA-Z ]+(?=, [:digit:]+)")



# YOUR TURN #5 ------------------------------------------------------------

# 1) In the quakes data frame, extract the distance from the place column and
# create a new column called "distance". For example, extract "7" from "7km NE
# of Kasukabe, Japan". Make sure the new "distance" column is numeric




# Detect presence or absence of pattern -----------------------------------

# The str_detect function returns TRUE or FALSE if a pattern is found.

# Example 1: return TRUE if number is toll-free 
p <- "^(800|888|877|866|855|844|833)"
str_detect(c("800-212-3433", "434-295-1888", "888-555-1212"), 
           pattern = p)

# Example 2: subset mcu data frame for Iron Man movies
subset(mcu, str_detect(movie, "Iron Man"))

# Example 3: proportion of states that end in "a"
state.name
mean(str_detect(state.name, "a$"))
# or using the pipe
str_detect(state.name, "a$") %>% mean()

# The str_subset function 

# Example 4: which states end in "a"; which end in "ia$"
str_subset(state.name, "a$")
str_subset(state.name, "ia$")


# YOUR TURN #6 ------------------------------------------------------------


# 1) In the injuries data frame, what proportion of reports had a Final
# Narrative mention "fell" or "fall"? 
str_detect(injuries$FinalNarrative, "(fell|fall)") %>% mean()

# 2) In the injuries data frame, how many reports had a Final Narrative that
# involved a "ladder"?
str_detect(injuries$FinalNarrative, "ladder") %>% sum()

# 3) In the quakes data frame, how many earthquakes occurred in California?
# (Note: California is logged either as "California" or "CA")
str_detect(quakes$place, "California$|CA$") %>% sum()


# End of workshop!


# Appendix: Time zones ----------------------------------------------------


# Above the letters "UTC" were been appended to the date-times. UTC is short for
# Universal Coordinated Time. You can read more about UTC at
# https://en.wikipedia.org/wiki/Coordinated_Universal_Time, but it's basically
# the time standard by which the world regulates clocks.

# We can specify a time zone when formatting dates by using the tz argument.
# Here's how we can specify the Eastern Time Zone in the United States when
# formatting our dates.

dateEx <-  c("7/8/97 8:00","10/23/02: 12:00","1/4/91 2:05")
mdy_hm(dateEx, tz = "US/Eastern")

# Notice the last date is EST instead of EDT. EST means "Eastern Standard Time".
# EDT means "Eastern Daylight Time". Any day and time that falls during Daylight
# Savings is EDT. Otherwise it's EST. How do we know the appropriate time zone 
# phrase to use in the tz argument? We can use the OlsonNames() function to see
# a character vector of all time zone names. Just enter OlsonNames() in the R 
# console and hit Enter.

OlsonNames()[1:5]
length(OlsonNames())

# We can see our current time zone with the Base R function Sys.timezone()
Sys.timezone() 

# We can combine an existing date-time with a new time zone using lubridate's
# with_tz() and force_tz().

# with_tz() returns a date-time as it would appear in a different time zone. What
# time is it right now in London?
(NOW <- now())
with_tz(NOW, tzone = "Europe/London")

# force_tz() returns a date-time that has the same clock time in the new time
# zone. This is useful for working with existing date-times for which you need
# to update the time zone without changing the times.
force_tz(NOW, tzone = "Europe/London")



# Appendix: Generating dates and times ------------------------------------


# It's fairly easy to generate a sequence of dates in R. Let's look at how to do
# it in both base R and lubridate.

# To illustrate let's create some fake data.
dat <- tibble(x = 1:5)

# Let's say we want to add a date in increments of one day starting on 
# "2017-01-01". Using lubridate we can specify a date and then add a certain
# number of days using either the days() or ddays() function, like so:

mutate(dat, date = ymd("2017-01-01") + ddays(0:4))
mutate(dat, date = ymd("2017-01-01") + days(0:4))

# days() creates a period object, while ddays() creates a duration object. In 
# this case it doesn't matter what we use. For more information see the Time
# Spans section of R for Data Science:
# http://r4ds.had.co.nz/dates-and-times.html#time-spans


# We could also do months and years:
mutate(dat, date = ymd("2017-01-01") + months(0:4))

mutate(dat, date = ymd("2017-01-01") + years(0:4))

# sequence of dates with interval of two weeks:
mutate(dat, date = ymd("2017-01-01") + weeks(0:4)*2)


# Appendix: Miscellaneous lubridate functions -----------------------------

# am(), pm() - Does date time occur in the am or pm?
am(now())
pm(now())

# leap_year() - is a year a leap year?
leap_year(2020)
leap_year(2000:2018)


# date rounding with round_date(), floor_date(), and ceiling_date()

# round_date() rounds to nearest value of specified time unit. 
# floor_date() rounds down to nearest boundary of specified time unit. 
# ceiling_date() rounds up to nearest boundary of specified time unit.
day1 <- now()
day1

round_date(day1, "month")
floor_date(day1, "month")
ceiling_date(day1, "month")

round_date(day1, "hour")
floor_date(day1, "hour")
ceiling_date(day1, "hour")

# Convert a date to a decimal of its year, where the date is expressed as a
# fraction of its year, with decimal_date()

day2 <- ymd("2017-07-01")
decimal_date(day2)  

# 2017.496 indicates that 2017-07-01 is about half-way through 2017.

# Likewise convert a decimal date to the actual date with date_decimal()
date_decimal(2017.5)

# Get the fiscal quarter and semester of a date-time with quater() and semester().

# Which quarter and semester do these dates fall in?
x <- ymd(c("2012-03-26", "2012-05-04", "2012-09-23", "2012-12-31"))
quarter(x)
quarter(x, with_year = TRUE)
semester(x)
semester(x, with_year = TRUE)




# Appendix: Convert a date into a character string ------------------------

# It's possible to convert a date to character format, and specify in what
# format we want the date to appear.

# Let's say we have the following dates, formatted as dates
days3 <- ymd("2002-04-03", "2010-08-23", "2017-07-22")
str(days3)

# One way to convert to character is to just use as.character()
days3_ch <- as.character(days3)
str(days3_ch)

# But what if we want the date to appear in a certain format, such as July 20, 
# 2017? We can use lubridate's stamp() function. This function is a little 
# unusual in the way it works. You provide it with a human-frendly template, 
# such as "Recorded at 10 am, September 2002" and then call it on a date object.
# Examples will hopefully make this clear. Here are the examples on the stamp
# help page.


# format days to look like "March 1, 1999"
stamp("March 1, 1999")(days3)

# create a stamp using the following template...
sf <- stamp("Created on Sunday, Jan 1, 1999 3:34 pm")

# now format our dates according to our stamp template
sf(days3)

# format dates to look like "Sun Aug 5"
stamp("Sun Aug 5")(days3)

# format dates to look like "12/31/99"
stamp("12/31/99")(days3)

# turn off the messages!
stamp("12/31/99", quiet = TRUE)(days3)

# We can also use the base R format function with strptime codes. See ?strptime 
# for codes.

format(days3, format = "%B %d, %Y")
format(days3, format = "%Y-%b-%d")




# Appendix: factors vs character data -------------------------------------

# factors are categorical data suitable for statistical modeling. They are
# integers with numeric labels.
gender_factor <- factor(c("Male","Male","Female"))
gender_factor
is.character(gender_factor)
str(gender_factor)

gender_char <- c("Male","Male","Female")
gender_char
is.character(gender_char)
str(gender_char)

# use as.character to transform a factor to character
as.character(gender_factor)

# Be careful when numbers are factors!
nums_factor <- factor(c(5,5,3,9))
str(nums_factor)
# Need to use as.character() and then as.numeric()
as.numeric(nums_factor) # probably not what we want!
as.numeric(as.character(nums_factor))

rm(gender_factor, gender_char, nums_factor)



# Appendix: Extract State and Location ------------------------------------

# In the quakes data, sometimes states are abbreviated, sometimes they're
# spelled out, but they appear to always come at the end of the string. This is
# good time to use R's built-in state.name and state.abb vectors.

state.name
state.abb

# Using paste with collapse="|" we can build a really big regular expression! 
# Notice we include the '$' to indicate the state name or abbreviation comes at
# the end of the string.
regx <- str_c('(',str_c(c(state.name, state.abb), collapse = "|"),')$')

# Now use it with str_extract to extract state
quakes$us.state <- str_extract(quakes$place, pattern = regx)

# Let's sample a few and check
quakes %>% 
  select(place, us.state) %>% 
  sample_n(10)

# How many non-missing do we have? (ie, how many earthquakes were in the US?)
sum(!is.na(quakes$us.state))

# proportion of earthquakes in US states:
mean(!is.na(quakes$us.state))

# Notice we have a mix of state names and abbreviations (well, just for CA)
table(quakes$us.state)

# Quick way to fix
quakes$us.state <- str_replace(quakes$us.state, 
                               pattern = "CA", 
                               replacement = "California")


# counts of earthquakes by state
table(quakes$us.state)
sort(table(quakes$us.state), decreasing = TRUE)
# state vs type
table(quakes$us.state, quakes$type)

# Extract the place

# Everything after "of " appears to be the place the earthquake was detected. We
# could extract the place to help us determine which place had the most
# earthquakes in our data.

# This is a good time for a look behind:
# Get one or more of everything that follows "of "
quakes$place2 <- str_extract(quakes$place, pattern = "(?<=of ).+")
# and replace CA with California
quakes$place2 <- str_replace(quakes$place2, 
                                pattern = "CA", 
                                replacement = "California")

# Which places had the most earthquakes?
sort(table(quakes$place2), decreasing = TRUE)[1:10]



# Appendix: current date and time -----------------------------------------

# lubridate also provides functions for getting the current date and time.
today() # date
now() # date-time

# Base R versions
Sys.Date() 
Sys.time() 



# Appendix: elapsed time between earthquakes ------------------------------

# Let's say we want to analyze elapsed time between earthquakes in Virginia.
# That means we need to create intervals between days there were earthquakes.

# Get earthquake data for Virginia
# We'll learn about str_detect in the 2nd half of workshop.
VAquakes <- subset(quakes, str_detect(quakes$place, "Virginia"))
nrow(VAquakes) # 42 earthquakes

# Now get intervals
quakeIntervals <- int_diff(VAquakes$time)
as.numeric(quakeIntervals)

# summarize intervals as days
time_length(quakeIntervals, "days")
time_length(quakeIntervals, "days") %>% summary()

# find largest interval
k <- time_length(quakeIntervals, "days") %>% which.max()
quakeIntervals[k]

