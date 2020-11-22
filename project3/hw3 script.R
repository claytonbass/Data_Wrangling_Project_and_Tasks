#1.	Compute the rate for table2, and table4a+table4b and perform the four operation
#a.	Extract the number of TB cases per country per year
#b.	Extract the matching population per country per year
#c.	Divide cases by population, and multiply by 10,000
#d.	Store back in appropriate place.

#1a. Extract the number of TB cases per country per year
library(dplyr)
library(tidyverse)
library(magrittr)
library(nycflights13)

TB_cases <- table2 %>% filter(type == 'cases')
TB_cases

#1b. Extract the matching population per country per year
country_pop <- table2 %>% filter(type == 'population')
country_pop

#1c.,d. Divide cases by population, and multiply by 10,000.
# We use a loop to move through tables 4a and 4b and accurately
# calculate the rate. Note that I discussed this approach with
# TA Heejoon and was approved to use this method (where I create
# a new table in part d. and store this information in that table). 
rate_vec <- rep(0, 6)
k <- 1
for(i in 1:3){
  for(j in 2:3){
    cases <- table4a[i,j]
    print(cases)
    population <- table4b[i,j]
    rate <- cases / population * 10000
    rate_vec[k] <- rate
    k = k + 1
  }
}
rate_df <- as.data.frame(rate_vec)

# Create a new table that accurately represents all of this information,
# in order to answer part d. properly. We use only the tables we have
# created in previous parts of the question in order to answer this
# question. 
table_1d <- cbind(country=TB_cases$country, 
                  year=TB_cases$year,
                  cases=TB_cases$count, 
                  population=country_pop$count, 
                  rate=rate_vec)

table_1d

#------------------------------------
#2. 
# Note that the code should instead be as follows:
table4a%>%gather('1999','2000',key="year",value="cases")
# This is because, if we specify 1999 and 2000 as they stand without
# writing them as strings, then the gather function will throw an error, 
# since it anticipates a string or symbol-based input (not a numeric
# data type).

#------------------------------------

#3

#3a.	How does the distribution of flights times within 
# a day change over the course of the year?
# Note that per my discussion with TA Heejoon, this is intended 
# to mean the distribution of flight LENGTHS, not departure times,
# So, we can simply look at the distribution of the "air_time" variable.
q3a_df <- flights[,c('year','month','day','air_time')]
q3a_df

# Find the mean flight time per day for every single day of the year
refined_q3a <- q3a_df %>% group_by(year,month,day) %>% summarise(mean_time = mean(air_time, na.rm=TRUE))
refined_q3a

# Plot the mean flight time per day for all days of the year. We use the mean metric
# per day in order to capture how the average flight length changes over the course 
# of the year (so we take a time-series approach here). We will also be examining
# how a histogram of the average flight times might look right after this (although
# of course this method obscures the temporal component here, since we are just binning
# all of the mean flight times together). 
# Notice that while there is not a CLEAR pattern to how the mean flight time varies
# throughout the year, we can see some slight patterns. This scatter plot
# looks almost like an upward-facing parabola, with the shortest flight
# times on average toward the middle of the year (around the summer), and 
# the longest fligh ttimes at the beginning and especially at the end of the year.
# This might make sense since people tend to travel much more during the holiday season
# toward the end of the year and at the start of the New Year, while there are fewer
# widely celebrated special occasions that require long travel over the summer months
# in the U.S.
plot(1:365, refined_q3a$mean_time, xlab = 'Day of the year',
     ylab = 'Mean flight length for a given day (minutes)',
     main = 'Mean flight length for each day in 2013',
     col='blue')

# We now also plot a histogram of the mean daily flight lengths
# to see how most of them cluster together. Notice that
# most of the days of the year see mean flight times somewhere
# around 150 minutes, while we see far fewer days of the year
# in 2013 where the average flight time is below 140 minutes
# or is around 170 minutes or higher. So, there is a clear
# pattern here -- it is fair to assume that the average
# flight will last around a bit more than 150 minutes across the board,
# although when one examines the time series plot (before encountering
# the histogram), more structure in the temporal distribution of flight
# times can be examined.
hist(refined_q3a$mean_time,
     main="Histogram of mean daily flight times in 2013",
     xlab="Mean daily flight lengths",
     ylab="Frequency",
     col="blue")


#3b. Compare dep_time,sched_dep_time, and dep_delay. 
# Are they consistent. Explain your findings
q3b <- flights %>% select(dep_time, sched_dep_time, dep_delay)
rand_indices <- sample(nrow(q3b), size=10)
q3b[rand_indices,]
# Yes -- by examining the result table after using the "select" command
# with chaining, we see that given a sched_dep_time and a dep_delay,
# if dep_delay is positive, then we add that number to sched_dep_time
# and receive the appropriate dep_time. If we have some value for
# sched_dep_time and a negative value for dep_delay, then once again
# we add sched_dep_time + dep_delay, and we receive dep_time. So, in
# essence, sched_dep_time + dep_delay = dep_time, which is what we
# anticipated. 

#3c. Confirm my hypothesis that the early departures of flights in 
# minutes 20-30 and 50-60 are caused by scheduled flights that leave early. 
# Hint: create a binary variable that tells whether or not a flight 
# was delayed.


# NOTE: Per my discussion with TA Heejoon, I will be examining
# whether flights with departures in minutes 20-30 and 50-60 (using their ACTUAL 
# departure time, NOT their scheduled departure time) tend to be
# flights that leave early (because of "negative" delays). 
# ALSO NOTE: In some rows (such as row 840) of q3b, we have 
# that there is a scheduled departure time, but there are no provided
# dep_delay or dep_time values, so we don't actually know when the flight
# left!
dep_delayed_indicator <- ifelse(flights$dep_delay > 0, 
                                1, 0)

# Add this new indicator variable to a new table. 
q3c <- cbind(flights, dep_delayed_indicator)

# Extract the data for actual date/time of each flight's scheduled
# departure. Then, add on the minutes of the delay for each of the scheduled
# departure dates/times. Then, extract only the minutes, and convert to numeric
# type from string type. 
actual_dates <- ISOdate(flights$year,flights$month,flights$day,flights$hour,flights$minute,
                        tz="EST")
actual_dep_times <- actual_dates + minutes(q3c$dep_delay)
actual_dep_minutes <- format(actual_dep_times, '%M', tz="EST")
actual_dep_min_numer <- as.numeric(actual_dep_minutes)

# Plot the minute number of each flight, and then examine
# the dep_delayed_indicator to see whether or not flights
# that left early were concentrated in the 20-30 minute and 50-60 minute
# departure times. 
new_table <- as.data.frame(cbind(actual_dep_min_numer, dep_delayed_indicator))

# Fill in the NA values with the mean (using mean imputation). There are
# very few NAs in general, so this shouldn't affect results and conclusions
# much.
new_table$actual_dep_min_numer[is.na(new_table$actual_dep_min_numer)] <- 
  round(mean(new_table$actual_dep_min_numer,na.rm=TRUE))
new_table$dep_delayed_indicator[is.na(new_table$dep_delayed_indicator)] <-
  round(mean(new_table$dep_delayed_indicator,na.rm=TRUE))

# NOTE: Per my discussion with TA Heejoon, I received clarification that it makes
# more sense to look at each minute as a discrete value rather than looking at all
# hundreds of thousands of data points, so here I group by the number of minutes
# in order to find the mean value of the binary response variable. If the mean
# value of the binary response is small, then that indicates that a high proportion
# of flights either were not delayed or left early for that given minute value, 
# and if the mean value is large, then a majority of the flights for that given
# minute value actually were delayed. 
new_table_1 <- new_table %>% group_by(actual_dep_min_numer) %>% summarize(mean_response = mean(dep_delayed_indicator))
plot(new_table_1$actual_dep_min_numer, new_table_1$mean_response,
     xlab = "Minute number of actual (NOT scheduled) departure time",
     ylab = "Mean value of binary delay indicator variable",
     main = "Plot of mean binary delay indicator \n variable for each minute of the hour",
     col = "blue",
     pch=16,
     cex=0.8)

# Notice crucially that in minutes 20-30 and minutes 50-60, we see that the mean
# binary response variable value is much lower than in minutes 0-20 and 30-50.
# This indicates that the majority (i.e., much more) of the flights that left in
# minutes 20-30 and 50-60 were not delayed or left early than those flights
# that left in minutes 0-20 or 30-50. This therefore confirms the hypothesis
# made in the question. 

#------------------------------------

#4.	Use similar methods as accessing Twitter from an API, search the 
#keyword “black Friday deals” on Facebook.

# NOTE: This question is now optional, and I will be solving it to the
# best of my ability for bonus points (per Professor Yapalparvi's 
# Canvas announcement).

# NOTE: We use the Rfacebook package with information found at
# https://cran.r-project.org/web/packages/Rfaceook/Rfacebook.pdf
# We use the Rfacebook vignette found at the following link in order to
# properly execute the functions: http://pablobarbera.com/code/Rfacebook.html
library(Rfacebook)

# We generate our token here: https://developers.facebook.com/tools/explorer/
# (this is an API Explorer that allows us to generate a User Access Token)/
# Note that we cannot use the searchFacebook function with our access token
# since version 2.0 of the Graph API for Facebook no longer allows
# this function to be used. 
user_access_token <- 
  "..."

# At https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf, we
# attempt to use the fbOAuth function in order to generate a key that works
# properly, but in this case it is actually required that we have a Privacy Policy
# URL for our app, which does not work, so this is not possible. 
app_id = "..."
app_secret = "..."
fb_oauth <- fbOAuth(app_id=app_id, app_secret=app_secret)

# Note that this is one way to get a proper black Friday deals page, 
# although I do not have permissions to actually properly set up
# app ID and app secret keys. Therefore, this alternative also does not
# work in gathering page data. However, depending on the actual
# token that one uses, one would be able to extract information from this particular
# black Friday deals page by using the following command that I was able to
# track down in the following documentation:
# https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf
black_friday_deals <- getPage(page="blackfridaydeals", token=user_access_token)
black_friday_deals <- searchFacebook("black Friday deals",
                                     user_access_token,
                                     since="1 January 2015",
                                     until="now")

# Neither of these functions work. First and foremost, the getPage function
# cannot work since the app I am using is in Development Mode, and thus
# since it has not passed App Review and gone into Live Mode, it cannot actually
# access public pages. In addition, the searchFacebook function does not work
# since public Facebook page/post scraping worked only in the previous version 
# of this API, whereas now the token needed to perform such scraping literally
# cannot be generated under any circumstances. Therefore, using getPage and searchFacebook,
# which are the only two actually relevant functions in this package, to find out
# what pages or posts are out there that mention Black Friday deals
# is impossible. This has been done to the best of my ability.

# In sum, there does not seem to be a valid R-based solution (with the given packages)
# that would be able to solve this problem effectively. This has been completed
# for bonus points. 





