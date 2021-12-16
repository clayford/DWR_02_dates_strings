# DWR_02 your turn answers


# YOUR TURN #1 ------------------------------------------------------------

# 1. Parse the EventDate column in the injuries data frame so it is stored as a
# Date (ie, number of days since 1/1/1970)
injuries$EventDate <- mdy(injuries$EventDate)

# 2. Parse the time and updated columns in the quakes data frame so they're
# stored as a Date-Time (ie, number of seconds since 1/1/1970). Hint: don't
# worry about letters in the dates. Just use what you think is the correct
# lubridate function.
quakes$time <- ymd_hms(quakes$time)
quakes$updated <- ymd_hms(quakes$updated)


# YOUR TURN #2 ------------------------------------------------------------

# 1. Add a column called "Day_of_Week" to the injuries data frame that indicates what
# day of week the injury occured on, such as "Friday" or "Saturday". 
injuries$Day_of_Week <- wday(injuries$EventDate, label = TRUE, abbr = FALSE)

# 2. Add a column called "Month" to the injuries data frame that indicates what
# month the injury happened in, such as "April" or "May"
injuries$Month <- month(injuries$EventDate, label = TRUE, abbr = FALSE)

# 3. CHALLENGE: Create a barplot to visualize the distributions of injuries over
# months. Hint: Use table() nested in barplot()
barplot(table(injuries$Month), cex.names = 0.75)
ggplot(injuries, aes(x = Month)) + geom_bar()



# YOUR TURN #3 ------------------------------------------------------------


# Examine the time between incidents for Virginia companies in the injuries data
# frame. The following creates a vector of dates for incidents in Virginia.
VAdates <- injuries$EventDate[injuries$State == "VIRGINIA"]

# What is mean amount of days between events in Virginia)?
time_length(int_diff(VAdates), "days") %>% summary()


# YOUR TURN #4 ------------------------------------------------------------

# 1) Modify the Employer column in the injuries data frame so that it has Title case
injuries$Employer <- str_to_title(injuries$Employer)

# 2) The Final Narrative column contains a description of how the injury
# occurred. To make the column easier to work with, change the Final Narrative
# column to consist of all lower case letters.
injuries$FinalNarrative <- tolower(injuries$FinalNarrative)

# 3) Fix the zip code column in the injuries data frame so that all zip codes
# have 5 digits and a leading 0 if necessary
injuries$Zip <- str_pad(injuries$Zip, width = 5, side = "left", pad = "0")



# YOUR TURN #5 ------------------------------------------------------------

# 1) In the quakes data frame, extract the distance from the place column and
# create a new column called "distance". For example, extract "7" from "7km NE
# of Kasukabe, Japan". Make sure the new "distance" column is numeric
quakes$distance <- as.numeric(str_extract(quakes$place, pattern = "^\\d+"))

# some checks
quakes %>% 
  select(place, distance) %>% 
  sample_n(size = 10)

summary(quakes$distance)

# investigate missings
quakes %>% 
  filter(is.na(distance)) %>% 
  select(place) %>% 
  as.data.frame()


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



