## Question 1

#1a.

# Read in the data
IC_BP_v2 <- read.csv(".../IC_BP_v2.csv")

# Convert BP alerts to BP status per part b.'s descriptions
IC_BP_v2$BPStatus <- ifelse(IC_BP_v2$BPAlerts == 'Hypo1' | IC_BP_v2$BPAlerts == 'Normal',
                            'Controlled blood pressure', 'Uncontrolled blood pressure')

# Print random 10 rows to show these changes
rand_indices <- sample(nrow(IC_BP_v2), size=10)
IC_BP_v2[rand_indices, ]


#1b. Turn controlled and uncontrolled blood pressure into dichotomous outcomes
IC_BP_v2$BPStatus_binary <- ifelse(IC_BP_v2$BPStatus == 'Controlled blood pressure',
                                   1, 0)

# Print random 10 rows to show these changes
rand_indices <- sample(nrow(IC_BP_v2), size=10)
IC_BP_v2[rand_indices, ]

#1c. Get the Demographics SQL table to merge and obtain enrollment dates

# With guidance from https://cran.r-project.org/web/packages/sqldf/README.html to use 
# sqldf properly
options(gsubfn.engine = "R")
library(sqldf)
library(RODBC)

# RUN THIS ONLY IN PARALLELS ON MAC. Write the relevant SQL tables to .csv files,
# and then load them in on Mac. [I exclude personal credentials here for the public GitHub repository.]
myconn <- odbcConnect("[...]",
                      "[...]",
                      "[...]")
Demographics <- sqlQuery(myconn,"select * from dbo.Demographics")
write.csv(Demographics,  ".../Demographics.csv")

# Run this part in Mac R
Demographics <- read.csv('.../Demographics.csv')

# Merge the two tables to obtain enrollment dates
bp_demographics <- sqldf("SELECT a.*, b.*
                         FROM IC_BP_v2 a
                         INNER JOIN Demographics b
                         on a.ID = b.contactid")

# Print random 10 rows to show these changes
rand_indices <- sample(nrow(bp_demographics), size=10)
bp_demographics[rand_indices,]

#1d. Create a 12-week interval of averaged scores of each customer.
# Note that it makes the most sense to treat a change of one unit in each customer's "ObservedTime" 
# as an increase in one day since enrollment. This is because in most cases, the difference between the maximum
# value of the ObservedTime for a given patient and the minimum value of the ObservedTime for that same patient
# is APPROXIMATELY the number of days in 12 weeks, although this does not line up in every instance. 
library(dplyr)

# Get rid of rows with no enrollment dates specified
bp_demographics_cleaned <- 
  bp_demographics[bp_demographics$tri_imaginecareenrollmentemailsentdate != 'NULL' &
                  bp_demographics$tri_enrollmentcompletedate != 'NULL',]

# We will create an interval of the average score of each patient for the first 12 weeks
# after their enrollment. First, create a new column in the dataset that represents the number of
# days since enrollment (assuming the first ObservedTime is Day 0 -- i.e., the same date as
# the tri_enrollmentcompletedate). 
bp_demographics_cleaned_2 <- 
  bp_demographics_cleaned %>% group_by(ID) %>% arrange(ID, 
                                                       desc(ObservedTime)) %>% mutate(DaysSinceEnrollment 
                                                                              = ObservedTime - min(ObservedTime))

# Now, create the actual date on which the blood pressure was recorded (assuming that we add on the number of Days SinceEnrollment to the given
# tri_enrollmentcompletedate). NOTE: Our critical assumption is that tri_enrollmentcompletedate is essentially "Day Zero" for the start of blood pressure
# recordings. With every increase in DaysSinceEnrollment by 1 (or every increase in ObservedTime by 1), we increase the ObservedDateTime by 1 day.
# The lubridate package is required in order to answer this question.
library(lubridate)
bp_demographics_cleaned_3 <- 
  bp_demographics_cleaned_2 %>% mutate(ObservedDateTime = 
                                         as.Date(tri_enrollmentcompletedate,format="%m/%d/%Y") + days(DaysSinceEnrollment))

# Add on a new column that represents the week number of the given blood pressure recording
bp_demographics_cleaned_3$ObservedWeek <- strftime(bp_demographics_cleaned_3$ObservedDateTime, format="%V")

# Now, take the mean value of the blood pressure recordings on a weekly basis (although we will keep only the first 12 values per ID since
# we need to make a 12-week interval). Please note that some patient IDs had significantly fewer than 12 total weeks recorded, so they will
# have fewer values represented ultimately. Thus, we must select only the bottom 12 values (since we want recordings from the first 12 weeks).
bp_demographics_cleaned_4 <- bp_demographics_cleaned_3[,c('BPStatus_binary','ID',
                            'ObservedWeek')] %>% group_by(ID, ObservedWeek) %>% summarize(AverageBPStatus 
                                                                  = mean(BPStatus_binary))

# We now need to subtract the minimum value of the ObservedWeek from all values per patient.
# This is so that we can eliminate all recordings that are past the 12-week mark easily in dplyr,
# and then ultimately use sqldf to filter out those patients who have fewer than 12 recordings (i.e.,
# those patients who do not have 12 weeks' worth of recordings). Note that per confirmation with
# Professor Yapalparvi, it is fine not to include all patients from the original dataset since some
# have very few (if any) recordings, and creating a 12-week interval of averaged scores for some patients
# therefore would not make any sense.

# Note that we use the filter function with documentation at https://dplyr.tidyverse.org/reference/filter.html.
# This table (bp_demographics_cleaned_5) represents the 12-week (or shorter) intervals, over each week of which
# we have taken the average score of the patient's controlled vs. uncontrolled blood pressure regime. Note that
# some of these patients do not have a full 12 weeks recorded, but we keep those patients present in this dataset
# anyway so as to prevent major data loss.
bp_demographics_cleaned_5 <- bp_demographics_cleaned_4 %>% mutate(WeeksSinceFirstRecording 
                            = as.numeric(ObservedWeek) - min(as.numeric(ObservedWeek))) %>% filter(WeeksSinceFirstRecording < 12)

# Show 10 random rows
rand_indices <- sample(nrow(bp_demographics_cleaned_5), size=10)
bp_demographics_cleaned_5[rand_indices,]

#1e. Compare the scores from baseline (first week) to follow-up scores (12 weeks). Note that here we look at WeeksSinceFirstRecording = 11
# since we start counting from 0. 
bp_demographics_cleaned_6 <- sqldf("select * from bp_demographics_cleaned_5 where WeeksSinceFirstRecording = 0 or WeeksSinceFirstRecording = 11")

# Select only those IDs where we have an entry at both week 0 and week 11 (afterwards, so 12 weeks total have been recorded)
bp_demographics_cleaned_7 <- sqldf("select * from bp_demographics_cleaned_6 where ID in (select ID from bp_demographics_cleaned_6 group by ID having count(ID) = 2)")

# Notice that in the data frame above, we see that in many cases, the value of AverageBPStatus
# is higher in the 12th week (week label 11) relative to the 1st week (week label 0), which indicates
# that, since a score of 1 indicates a controlled regimen, on average we would expect to see
# controlled regimens emerge after 12 weeks, although in some cases, like for patient 
# [...] (OMITTED DUE TO PUBLIC AVAILABILITY), the patient already started with a controlled regimen,
# and ultimately ended up with a controlled regimen anyway. 
# PRINT 10 RANDOM ROWS TO SHOW THESE CHANGES:
rand_indices <- sample(nrow(bp_demographics_cleaned_7), size=10)
bp_demographics_cleaned_7[rand_indices,]

# We now plot the difference between the AverageBPStatus values in weeks 1 and 12 per patientID
# to get a sense of how large these changes tend to be. See ?diff for documentation on the base
# R function "diff"; notice that the same pattern as previously mentioned, with many of the differences
# being significantly positive (and sometimes even exactly equal to 1, indicating a perfect change from
# uncontrolled to controlled blood pressure regimes). Note that we do not need to include
# anything on the x-axis since only patient IDs would be listed, which would get very cluttered.
# (The actual ordering of them on the x-axis does not matter; only general trends matter for answering
# the question here. Recovering the IDs is not hard, either; one simply needs to specify "axes=T" in
# the plot command, or nothing at all, since by default the entry is assumed to be "axes=T".)
diff_table <- bp_demographics_cleaned_7 %>% group_by(ID) %>% mutate(diffs = diff(AverageBPStatus))
condensed_table <- diff_table %>% filter(WeeksSinceFirstRecording == 0)
plot(1:15, condensed_table$diffs, las=2, cex.axis = 0.7, cex.lab=0.7,
     xlab = "", ylab = "Difference in Avg. BP Score from Week 1 to Week 12",
     main = "Comparing Average BP Status Binary Score Values Between Week 1 and Week 12 \n by Examining the Difference from Week 1 to Week 12",
     cex.main = 0.7, axes=F, pch=16, col="blue", cex=0.8)
axis(side=2)
condensed_table$diffs
#1f. Count the number of positive differences that we see in order to find out how
# many patients moved from uncontrolled to controlled regime between weeks 1 and 12.
count <- 0
for(i in 1:nrow(condensed_table)){
  if(as.numeric(condensed_table$diffs[i]) > 0){
    count <- count + 1
  }
}
count

# Notice that the count is 9. We extrapolate and note that
# we have the following proportion of patients who had a positive change in their
# average BP score:
count / nrow(condensed_table)

# Notice that 0.6 of the patients saw an increase in their average BP score.
# This does not necessarily mean that they are exactly in a controlled regimen,
# since there are still some days where they have a score of 0 in their binary
# indicator for a controlled vs. uncontrolled regimen, but this at least indicates
# that the majority of patients are headed in the right direction in this regard.
# More data points are needed in order to ensure that this trend actually holds
# in larger sample sizes and generalizes properly. 

# To answer the question more explicitly, we look at diff_table (as below),
# and we see that there are 7 patients who were at an average score value
# of below 1, and who were brought to an average score value of exactly 1
# in the 12th week of intervention. 
rand_indices <- sample(nrow(diff_table), size=10)
as.data.frame(diff_table[rand_indices, ])

#--------

# ALSO NOTE: There is another possible interpretation of the question here
# that is perhaps more lenient. Some patients have recordings in the 11th or 13th week
# but for some reason do not have recordings available exactly in the 12th week. This may
# or may not be a clinically significant difference ultimately, but regardless, another way
# to view this question would be to look at the 12 earliest recordings of average 
# blood pressure scores (since it's possible that patients received support/help/assistance
# only in the weeks where they had recordings made, so in essence they still tallied a total
# of 12 weeks of help). We write this code here as our secondary answer
# (PLEASE NOTE THAT THIS ANSWERS QUESTIONS 1e. AND 1f.):

#1e. (alternate interpretation)
bp_demographics_alternate <- bp_demographics_cleaned_4 %>% mutate(WeeksSinceFirstRecording 
                                     = as.numeric(ObservedWeek) 
                                     - min(as.numeric(ObservedWeek))) %>% top_n(-12, WeeksSinceFirstRecording)

# We now must select the rows corresponding to AverageBPStatus values where the WeeksSinceFirstRecording
# values are at a minimum or a maximum. Create tables for the minimum and maximum week, respectively.
bp_alt_max <- bp_demographics_alternate %>% group_by(ID) %>% top_n(1, WeeksSinceFirstRecording)
bp_alt_min <- bp_demographics_alternate %>% group_by(ID) %>% top_n(-1, WeeksSinceFirstRecording)

# Union both of the tables.
bp_alt <- sqldf("select * from bp_alt_max 
                UNION
                select * from bp_alt_min")

# Take only the IDs that have two recordings present (since if only one
# recording is present, then only one week's worth of blood pressure recordings
# was made). 
bp_alt_cleaned <- 
  sqldf("select * from bp_alt where ID in (select ID from bp_alt group by ID having count(ID) = 2)")

# Notice again that in many cases, we see the value of the 
# AverageBPStatus column increase from the start of treatment
# up until the end of treatment (even if technically that spans more
# or fewer than 12 weeks -- regardless, the first 12 weeks
# of recorded blood pressure are the only ones that are recorded,
# even if they are technically not contiguous weeks; this is not
# specified as a requirement in the question, so I am justifying it here
# in this alternate solution, since it could be clinically significant,
# but of course there is no way to be sure of this only within the bounds
# of this final examination)
rand_indices <- sample(nrow(bp_alt_cleaned), size=10)
bp_alt_cleaned[rand_indices,]

# We now plot the difference between the AverageBPStatus values in the initial and final weeks of treatment
# per patientID to get a sense of how large these changes tend to be. See ?diff for documentation on the base
# R function "diff"; notice that the same pattern as previously mentioned, with many of the differences
# being significantly positive (and sometimes even exactly equal to 1, indicating a perfect change from
# uncontrolled to controlled blood pressure regimes). 
# Note that we do not need to include
# anything on the x-axis since only patient IDs would be listed, which would get very cluttered.
# (The actual ordering of them on the x-axis does not matter; only general trends matter for answering
# the question here. Recovering the IDs is not hard, either; one simply needs to specify "axes=T" in
# the plot command, or nothing at all, since by default the entry is assumed to be "axes=T".)
diff_table_alt <- bp_alt_cleaned %>% group_by(ID) %>% mutate(diffs = diff(AverageBPStatus))
condensed_table_alt <- diff_table_alt %>% filter(WeeksSinceFirstRecording == 0)
plot(1:94, condensed_table_alt$diffs, las=2, cex.axis = 0.5, cex.lab=0.7,
     xlab = "", ylab = "Difference in Avg. BP Score from Week 1 to 12th (Non-Contiguous) Week",
     main = 
     "Comparing Average BP Status Binary Score Values Between First and Final Weeks \n by Examining the Difference from First to Final Week of Recording",
     cex.main = 0.7, axes=F, pch=16, col="blue", cex = 0.6)
axis(side=2)

#1f. (ALTERNATE INTERPRETATION)
count_alt <- 0
for(i in 1:nrow(condensed_table_alt)){
  if(as.numeric(condensed_table_alt$diffs[i]) > 0){
    count_alt <- count_alt + 1
  }
}
count_alt

# Notice that the count is 44. We extrapolate and note that
# we have the following proportion of patients who had a positive change in their
# average BP score:
count_alt / nrow(condensed_table_alt)

# Notice that 0.47 of the patients saw an increase in their average BP score.
# This does not necessarily mean that they are exactly in a controlled regimen,
# since there are still some days where they have a score of 0 in their binary
# indicator for a controlled vs. uncontrolled regimen, but this at least indicates
# that a sizable proportion of patients are headed in the right direction in this regard.
# More data points are needed in order to ensure that this trend actually holds
# in larger sample sizes and generalizes properly. This is a smaller proportion
# than what was seen when strictly adhering to those patients who had 
# recordings taken exactly in week 1 and week 12 of their blood pressure monitoring,
# which is not surprising since we've moved to a much larger set of data
# with more variability.

# To answer the question more explicitly, we look at diff_table_alt (as below),
# and we see that there are 30 patients who were at an average score value
# of below 1, and who were brought to an average score value of exactly 1
# in the 12th overall (i.e., not necessarily contiguous) week of intervention. 
rand_indices <- sample(nrow(diff_table_alt), size=10)
as.data.frame(diff_table_alt[rand_indices,])

#----------
# Question 2
# THIS HAS BEEN COMPLETED IN SSMS ON PARALLELS. THE SQL CODE SCREENSHOTS 
# AND OUTPUT ARE PASTED IN THE RMARKDOWN FILE.

# THE RELEVANT R CODE IS HERE:
# See https://bookdown.org/yihui/bookdown/figures.html for documentation
# on knitr's include_graphics function
knitr::include_graphics('.../sql1.png')
knitr::include_graphics('.../sql2.png')

knitr::include_graphics('.../pic1.png')
knitr::include_graphics('.../pic2.png')


#----------
# Question 3

# RUN THIS ONLY IN PARALLELS ON MAC. Write the relevant SQL tables to .csv files,
# and then load them in on Mac.
Conditions <- sqlQuery(myconn,"select * from dbo.Conditions")
Text <- sqlQuery(myconn,"select * from dbo.Text")
write.csv(Conditions,  ".../Conditions.csv")
write.csv(Text,  ".../Final.csv")

# Run this part on Mac R
Conditions <- read.csv('.../Conditions.csv')
Text <- read.csv('.../Text.csv')

# Merge the tables Demographics, Conditions, and TextMessages
intermediate_df <- merge(Demographics, Conditions,
                         by.x = "contactid",
                         by.y = "tri_patientid")
q3_df <- merge(intermediate_df, Text,
               by.x = "contactid",
               by.y = "tri_contactId")

# We use the top_n function from dplyr with documentation at 
# https://dplyr.tidyverse.org/reference/top_n.html (or simply using ?top_n in
# the RStudio Console)
q3_df_next <- q3_df %>% group_by(contactid) %>% top_n(1, TextSentDate)
q3_df_next[,c('contactid','TextSentDate')]

# Print 10 random rows
rand_indices <- sample(nrow(q3_df_next), size=10)
q3_df_next[rand_indices, ]

#----------
# Question 4
# The link to my public GitHub repo is here: 
# https://github.com/claytonbass/Data_Wrangling_Project_and_Tasks
