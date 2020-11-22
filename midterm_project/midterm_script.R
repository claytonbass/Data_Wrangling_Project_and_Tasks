# Use the haven library (at https://haven.tidyverse.org/reference/read_xpt.html) in order to
# read in the .XPT file

# Load the raw DataFrame; keep an original copy of the given data table
# in case errors are made and we have to back-track.
library(haven)
original_df <- read_xpt(".../DIQ_I.XPT")
raw_df <- read_xpt(".../DIQ_I.XPT")

# In the DIQ010 column
# Find the proportion of missing values
sum(is.na(raw_df$DIQ010))/nrow(raw_df) #0! So we're good. We just create a new labeled column.
raw_df$DIQ010_label <- ifelse(raw_df$DIQ010 == 1, "Yes",
                              ifelse(raw_df$DIQ010 == 2, "No",
                                     ifelse(raw_df$DIQ010 == 3, "Borderline",
                                            ifelse(raw_df$DIQ010 == 7, "Refused",
                                                   ifelse(raw_df$DIQ010 == 9, "Don't know", "No data")))))


# For the DID060/DIQ060U columns: we try to specify the month and year 
# when a patient started taking insulin (we do this before altering the 
# tables significantly or removing any rows). We make the assumption that the data were recorded
# on 1/1/2016, since we don't really have more information here. This column would
# not be used in reality if this assumption were not true in practice; regardless, 
# it is an approximation. 
library(lubridate)

raw_df$DID060_date <- rep(NA, nrow(raw_df))
for(i in 1:nrow(raw_df)){
  if(!is.na(raw_df$DIQ060U[i])){
  if(as.numeric(raw_df$DIQ060U[i]) == 1 & is.numeric(raw_df$DID060[i])){
    number <- round(as.numeric(raw_df$DID060[i]))
    raw_df$DID060_date[i] = as.character(year(as.POSIXlt('2016-01-01') - months(number)))
  }
  else if(as.numeric(raw_df$DIQ060U[i]) == 2 & is.numeric(raw_df$DID060[i])){
    number <- round(as.numeric(raw_df$DID060[i]))
    raw_df$DID060_date[i] = as.character(as.POSIXlt('2016-01-01') - years(number))
  }}}
raw_df$DID060_date[is.na(raw_df$DID060_date) &
                     raw_df$DID060 == 666] <- 'Less than 1 month'
raw_df$DID060_date[is.na(raw_df$DID060_date) &
                     raw_df$DID060 == 777] <- 'Refused'
raw_df$DID060_date[is.na(raw_df$DID060_date) &
                     raw_df$DID060 == 999] <- "Don't know"
raw_df$DID060_date[is.na(raw_df$DID060_date)] <- "No data"

# In the DID040 column, create a truly numerical variable, and 
# perform mean imputation. Don't impute on the values that should be changed
# to labels. 
# We use the "floor" function here since it makes sense in the context of
# other ages provided. This is an age that we'd expect to see "on average" in the
# given dataset.
raw_df$DID040[is.na(raw_df$DID040)] <- floor(mean(raw_df$DID040[!is.na(raw_df$DID040)
                                                                & raw_df$DID040 != 999
                                                                & raw_df$DID040 != 666
                                                                & raw_df$DID040 != 777]))
raw_df$DID040[raw_df$DID040 == 999] <- "Don't know"
raw_df$DID040[raw_df$DID040 == 777] <- "Refused"
raw_df$DID040[raw_df$DID040 == 666] <- "Less than 1 year"

# In the DIQ160 column: this is categorical, so we replace values with the mode
# Find the proportion of missing values
sum(is.na(raw_df$DIQ160))/nrow(raw_df) #0.3687, so we impute the mode
library(modeest) #we use the library modeest to calculate mode, since mode() doesn't exist in R
# (well, mode() exists in R, but it doesn't calculate the statistical mode)
# Calculate the mode of the given values using the mfv() function in modeest library
mode_DIQ160 <- mfv(raw_df$DIQ160[!is.na(raw_df$DIQ160)])
raw_df$DIQ160[is.na(raw_df$DIQ160)] <- mode_DIQ160

# Add column labels for the DIQ160 data -- the value "No data" is not actually
# inputted since we did mean imputation already
raw_df$DIQ160_label <- ifelse(raw_df$DIQ160 == 1, "Yes",
                              ifelse(raw_df$DIQ160 == 2, "No",
                                     ifelse(raw_df$DIQ160 == 7, "Refused",
                                            ifelse(raw_df$DIQ160 == 9, "Don't know", "No data"))))

# In the DIQ170 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df$DIQ170))/nrow(raw_df) #0.354, so we impute the mode
mode_DIQ170 <- mfv(raw_df$DIQ170[!is.na(raw_df$DIQ170)])
raw_df$DIQ170[is.na(raw_df$DIQ170)] <- mode_DIQ170

# Add a column label for the DIQ170 data -- the value "No data" is not actually
# inputted since we did mean imputation already
raw_df$DIQ170_label <- ifelse(raw_df$DIQ170 == 1, "Yes",
                              ifelse(raw_df$DIQ170 == 2, "No",
                                     ifelse(raw_df$DIQ170 == 7, "Refused",
                                            ifelse(raw_df$DIQ170 == 9, "Don't know", "No data"))))

# In the DIQ172 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df$DIQ172))/nrow(raw_df) #0.354, so we impute the mode
mode_DIQ172 <- mfv(raw_df$DIQ172[!is.na(raw_df$DIQ172)])
raw_df$DIQ172[is.na(raw_df$DIQ172)] <- mode_DIQ172

# Add column labels for the DIQ172 data -- the value "No data" is not actually
# inputted since we did mean imputation already
raw_df$DIQ172_label <- ifelse(raw_df$DIQ172 == 1, "Yes",
                              ifelse(raw_df$DIQ172 == 2, "No",
                                     ifelse(raw_df$DIQ172 == 7, "Refused",
                                            ifelse(raw_df$DIQ172 == 9, "Don't know", "No data"))))


# Create a new column to represent all answers to the given question for all
# columns DIQ175A through DIQ175X. These columns contain so many missing values
# since they are literally just answering the same question, and it's extremely unlikely
# that any one person would put down all (or almost all) of the reasons given for why
# someone is at diabetes risk. We first convert all of these columns to categorical variables. 
raw_df$DIQ175A <- ifelse(raw_df$DIQ175A == 10, "Family history",
                         ifelse(raw_df$DIQ175A == 77, "Refused",
                                ifelse(raw_df$DIQ175A == 99, "Don't know", NA)))
raw_df$DIQ175B <- ifelse(raw_df$DIQ175B == 11, "Overweight", NA)
raw_df$DIQ175C <- ifelse(raw_df$DIQ175C == 12, "Age", NA)
raw_df$DIQ175D <- ifelse(raw_df$DIQ175D == 13, "Poor diet", NA)
raw_df$DIQ175E <- ifelse(raw_df$DIQ175E == 14, "Race", NA)
raw_df$DIQ175F <- ifelse(raw_df$DIQ175F == 15, "Had a baby weighed over 9 lbs. at birth", NA)
raw_df$DIQ175G <- ifelse(raw_df$DIQ175G == 16, "Lack of physical activity", NA)
raw_df$DIQ175H <- ifelse(raw_df$DIQ175H == 17, "High blood pressure", NA)
raw_df$DIQ175I <- ifelse(raw_df$DIQ175I == 18, "High blood sugar", NA)
raw_df$DIQ175J <- ifelse(raw_df$DIQ175J == 19, "High cholesterol", NA)
raw_df$DIQ175K <- ifelse(raw_df$DIQ175K == 20, "Hypoglycemic", NA)
raw_df$DIQ175L <- ifelse(raw_df$DIQ175L == 21, "Extreme hunger", NA)
raw_df$DIQ175M <- ifelse(raw_df$DIQ175M == 22, "Tingling/numbness in hands or feet", NA)
raw_df$DIQ175N <- ifelse(raw_df$DIQ175N == 23, "Blurred vision", NA)
raw_df$DIQ175O <- ifelse(raw_df$DIQ175O == 24, "Increased fatigue", NA)
raw_df$DIQ175P <- ifelse(raw_df$DIQ175P == 25, "Anyone could be at risk", NA)
raw_df$DIQ175Q <- ifelse(raw_df$DIQ175Q == 26, "Doctor warning", NA)
raw_df$DIQ175R <- ifelse(raw_df$DIQ175R == 27, "Other, specify", NA)
raw_df$DIQ175S <- ifelse(raw_df$DIQ175S == 28, "Gestational diabetes", NA)
raw_df$DIQ175T <- ifelse(raw_df$DIQ175T == 29, "Frequent urination", NA)
raw_df$DIQ175U <- ifelse(raw_df$DIQ175U == 30, "Thirst", NA)
raw_df$DIQ175V <- ifelse(raw_df$DIQ175V == 31, "Craving for sweet/eating a lot of sugar", NA)
raw_df$DIQ175W <- ifelse(raw_df$DIQ175W == 32, "Medication", NA)
raw_df$DIQ175X <- ifelse(raw_df$DIQ175X == 33, "Polycystic ovarian syndrome", NA)

# Now, combine all of the columns into one, but discard any NA
# values in the process. We concatenate strings together if there
# are multiple answers given by the same person to the given question.
library(tidyr)
new_df <- raw_df[, 7:30]
new_new <- unite(new_df, '', sep=', ',na.rm=TRUE)
new_new_df <- as.data.frame(new_new)
raw_df$DIQ175 <- new_new_df
raw_df$DIQ175[raw_df$DIQ175 == ''] = "No answer provided"

# Get rid of the now-unneeded columns DIQ175A through DIQ175X
raw_df_updated <- subset(raw_df, select=-c(7:30))
colnames(raw_df_updated)[colnames(raw_df_updated) == "DIQ175"] <- "DIQ175_answers"

# Now, back to updating columns.

# In the DIQ180 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df_updated$DIQ180))/nrow(raw_df_updated) #0.354, so we impute the mode
mode_DIQ180 <- mfv(raw_df_updated$DIQ180[!is.na(raw_df_updated$DIQ180)])
raw_df_updated$DIQ180[is.na(raw_df_updated$DIQ180)] <- mode_DIQ180

# Add a column label for the DIQ180 data 
raw_df_updated$DIQ180_label <- ifelse(raw_df_updated$DIQ180 == 1, "Yes",
                                      ifelse(raw_df_updated$DIQ170 == 2, "No",
                                             ifelse(raw_df_updated$DIQ170 == 7, "Refused",
                                                    ifelse(raw_df_updated$DIQ170 == 9, "Don't know", "No data"))))


# In the DIQ050 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df_updated$DIQ050))/nrow(raw_df_updated) #No missing values, so nothing to impute!

# Add a column label for the DIQ050 data -- the value "No data" is not actually
# inputted since we did mean imputation already
raw_df_updated$DIQ050_label <- ifelse(raw_df_updated$DIQ050 == 1, "Yes",
                                      ifelse(raw_df_updated$DIQ050 == 2, "No",
                                             ifelse(raw_df_updated$DIQ050 == 7, "Refused",
                                                    ifelse(raw_df_updated$DIQ050 == 9, "Don't know", "No data"))))


# In the DID060 column:
sum(is.na(raw_df_updated$DID060))/nrow(raw_df_updated) #97.3% of values are missing, so we don't impute
# Just replace the NAs with "No data" since no data is available, and we don't want to impute
# or remove a very large number of rows
raw_df_updated$DID060[is.na(raw_df_updated$DID060)] <- "No data"

#We now combine this column with the DIQ060U table (which gives units of
# months or years). Note that the count of values for
# "Range of values" 1-55 (in DIQ060) and for "Months" and "Years" 
# (in DIQ060U) is equal in both cases, so we don't have any time periods
# longer than 1 month that don't have a unit attached.

raw_df_updated$DIQ060U <- ifelse(raw_df_updated$DIQ060U == 1, "Months",
                                 ifelse(raw_df_updated$DIQ060U == 2, "Years", "No data"))

new_df_2 <- raw_df_updated[, 9:10]
new_new_2 <- unite(new_df_2, '', sep=' ',na.rm=TRUE)
new_new_df_2 <- as.data.frame(new_new_2)
raw_df_updated$DID060_units <- new_new_df_2
raw_df_updated_2 <- subset(raw_df_updated, select=-c(9,10))


# In the DIQ070 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df_updated_2$DIQ070))/nrow(raw_df_updated_2) #0.8418, so we do not impute
raw_df_updated_2$DIQ070[is.na(raw_df_updated_2$DIQ070)] <- "No data"

# Add column labels for the DIQ172 data
raw_df_updated_2$DIQ070_label <- ifelse(raw_df_updated_2$DIQ070 == 1, "Yes",
                                        ifelse(raw_df_updated_2$DIQ070 == 2, "No",
                                               ifelse(raw_df_updated_2$DIQ070 == 7, "Refused",
                                                      ifelse(raw_df_updated_2$DIQ070 == 9, "Don't know", "No data"))))

# In the DIQ230 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df_updated_2$DIQ230))/nrow(raw_df_updated_2) #0.9109, so we do not impute
raw_df_updated_2$DIQ230[is.na(raw_df_updated_2$DIQ230)] <- "No data"

# Add column labels for the DIQ230 data
raw_df_updated_2$DIQ230_label <- ifelse(raw_df_updated_2$DIQ230 == 1, "< 1 year ago",
                                        ifelse(raw_df_updated_2$DIQ230 == 2, "> 1 year ago, <= 2 yrs. ago",
                                               ifelse(raw_df_updated_2$DIQ230 == 3, "> 2 years ago, <= 5 yrs. ago",
                                                      ifelse(raw_df_updated_2$DIQ230 == 4, "> 5 years ago", 
                                                             ifelse(raw_df_updated_2$DIQ230 == 5, "Never",
                                                                    ifelse(raw_df_updated_2$DIQ230 == 7, "Refused",
                                                                           ifelse(raw_df_updated_2$DIQ230 == 9, "Don't know", "No data")))))))



# In the DIQ240 column: this is categorical, so we check if we should impute
# the mode
sum(is.na(raw_df_updated_2$DIQ240))/nrow(raw_df_updated_2) #0.9109, so we do not impute
raw_df_updated_2$DIQ240[is.na(raw_df_updated_2$DIQ240)] <- "No data"

# Add column labels for the DIQ240 data
raw_df_updated_2$DIQ240_label <- ifelse(raw_df_updated_2$DIQ240 == 1, "Yes",
                                        ifelse(raw_df_updated_2$DIQ240 == 2, "No",
                                               ifelse(raw_df_updated_2$DIQ240 == 7, "Refused",
                                                      ifelse(raw_df_updated_2$DIQ240 == 9, "Don't know", "No data"))))



# In the DID250 column: this is numerical. We impute the mean. We rename
# values like 7777 and 9999 with their proper labels, since they have important
# meaning on their own. (Otherwise, if we replace these values with NAs in the 
# relevant rows, we are just getting rid of data that we already have access to
# and are not truly trying to impute.)
raw_df_updated_2$DID250[is.na(raw_df_updated_2$DID250)] <- floor(mean(raw_df_updated_2$DID250[!is.na(raw_df_updated_2$DID250)
                                                                                              & raw_df_updated_2$DID250 != 7777
                                                                                              & raw_df_updated_2$DID250 != 9999]))
raw_df_updated_2$DID250[raw_df_updated_2$DID250 == 7777] <- "Refused"
raw_df_updated_2$DID250[raw_df_updated_2$DID250 == 9999] <- "Don't know"



# Combine the DID260 and DIQ260U columns (just as was done with the
# DID060 and DIQ060U columns)
raw_df_updated_2$DID260[raw_df_updated_2$DID260 == 777] <- "Refused"
raw_df_updated_2$DID260[raw_df_updated_2$DID260 == 999] <- "Don't know"
raw_df_updated_2$DID260[is.na(raw_df_updated_2$DID260)] <- "No data"

raw_df_updated_2$DIQ260U <- ifelse(raw_df_updated_2$DIQ260U == 1, "per day",
                                   ifelse(raw_df_updated_2$DIQ260U == 2, "per week",
                                          ifelse(raw_df_updated_2$DIQ260U == 3, "per month", 
                                                 ifelse(raw_df_updated_2$DIQ260U == 4, "per year", "No data")))) 
new_df_3 <- raw_df_updated_2[, 13:14]
new_new_3 <- unite(new_df_3, '', sep=' ',na.rm=TRUE)
new_new_df_3 <- as.data.frame(new_new_3)
raw_df_updated_2$DID260_units <- new_new_df_3
raw_df_updated_3 <- subset(raw_df_updated_2, select=-c(13,14))
raw_df_updated_3$DID260_units[raw_df_updated_3$DID260_units == ''] <- 'No data'

# For the column DIQ275: this is categorical, so we check if we should
# impute the mode. 
sum(is.na(raw_df_updated_3$DIQ275))/nrow(raw_df_updated_3) #0.9109, so we do not impute
raw_df_updated_3$DIQ275[is.na(raw_df_updated_3$DIQ275)] <- "No data"

raw_df_updated_3$DIQ275_label <- ifelse(raw_df_updated_3$DIQ275 == 1, "Yes",
                                        ifelse(raw_df_updated_3$DIQ275 == 2, "No",
                                               ifelse(raw_df_updated_3$DIQ275 == 7, "Refused", 
                                                      ifelse(raw_df_updated_3$DIQ275 == 9, "Don't know", "No data")))) 


# For the column DIQ280: this is a numerical variable. Therefore, we
# aim to impute the mean. 
raw_df_updated_3$DIQ280[is.na(raw_df_updated_3$DIQ280)] <- round(mean(raw_df_updated_3$DIQ280[!is.na(raw_df_updated_3$DIQ280)
                                                                                              & raw_df_updated_3$DIQ280 != 999
                                                                                              & raw_df_updated_3$DIQ280 != 777]),1)
raw_df_updated_3$DIQ280[raw_df_updated_3$DIQ280 == 777] <- "Refused"
raw_df_updated_3$DIQ280[raw_df_updated_3$DIQ280 == 999] <- "Don't know"


# For the column DIQ291: this is categorical, so we check if we should
# impute the mode. 
sum(is.na(raw_df_updated_3$DIQ291))/nrow(raw_df_updated_3) #0.93305, so we do not impute
raw_df_updated_3$DIQ291[is.na(raw_df_updated_3$DIQ291)] <- "No data"

raw_df_updated_3$DIQ291_label <- ifelse(raw_df_updated_3$DIQ291 == 1, "<6",
                                        ifelse(raw_df_updated_3$DIQ291 == 2, "<7",
                                               ifelse(raw_df_updated_3$DIQ291 == 3, "<8", 
                                                      ifelse(raw_df_updated_3$DIQ291 == 4, "<9", 
                                                             ifelse(raw_df_updated_3$DIQ291 == 5, "<10", 
                                                                    ifelse(raw_df_updated_3$DIQ291 == 6, "Provider didn't specify goal", 
                                                                           ifelse(raw_df_updated_3$DIQ291 == 77, "Refused", 
                                                                                  ifelse(raw_df_updated_3$DIQ291 == 99, "Don't know", "No data")))))))) 


# For the column DIQ300S: this is a numerical variable. Therefore, we
# aim to impute the mean. 
raw_df_updated_3$DIQ300S[is.na(raw_df_updated_3$DIQ300S)] <- round(mean(raw_df_updated_3$DIQ300S[!is.na(raw_df_updated_3$DIQ300S)
                                                                                                 & raw_df_updated_3$DIQ300S != 7777
                                                                                                 & raw_df_updated_3$DIQ300S != 9999]))
raw_df_updated_3$DIQ300S[raw_df_updated_3$DIQ300S == 7777] <- "Refused"
raw_df_updated_3$DIQ300S[raw_df_updated_3$DIQ300S == 9999] <- "Don't know"


# For the column DIQ300D: this is a numerical variable. Therefore, we
# aim to impute the mean. 
raw_df_updated_3$DIQ300D[is.na(raw_df_updated_3$DIQ300D)] <- round(mean(raw_df_updated_3$DIQ300D[!is.na(raw_df_updated_3$DIQ300D)
                                                                                                 & raw_df_updated_3$DIQ300D != 7777
                                                                                                 & raw_df_updated_3$DIQ300D != 9999]))
raw_df_updated_3$DIQ300D[raw_df_updated_3$DIQ300D == 7777] <- "Refused" 
raw_df_updated_3$DIQ300D[raw_df_updated_3$DIQ300D == 9999] <- "Don't know"

# For the column DID310S: this is a numerical variable. Therefore, we
# aim to impute the mean. 
raw_df_updated_3$DID310S[is.na(raw_df_updated_3$DID310S)] <- round(mean(raw_df_updated_3$DID310S[!is.na(raw_df_updated_3$DID310S)
                                                                                                 & raw_df_updated_3$DID310S != 6666
                                                                                                 & raw_df_updated_3$DID310S != 7777
                                                                                                 & raw_df_updated_3$DID310S != 9999]))
raw_df_updated_3$DID310S[raw_df_updated_3$DID310S == 6666] <- "Provider didn't specify goal"
raw_df_updated_3$DID310S[raw_df_updated_3$DID310S == 7777] <- "Refused"
raw_df_updated_3$DID310S[raw_df_updated_3$DID310S == 9999] <- "Don't know"


# For the column DID310D: this is a numerical variable. Therefore, we
# aim to impute the mean. 
raw_df_updated_3$DID310D[is.na(raw_df_updated_3$DID310D)] <- round(mean(raw_df_updated_3$DID310D[!is.na(raw_df_updated_3$DID310D)
                                                                                                 & raw_df_updated_3$DID310D != 6666
                                                                                                 & raw_df_updated_3$DID310D != 7777
                                                                                                 & raw_df_updated_3$DID310D != 9999]))
raw_df_updated_3$DID310D[raw_df_updated_3$DID310D == 6666] <- "Provider didn't specify goal"
raw_df_updated_3$DID310D[raw_df_updated_3$DID310D == 7777] <- "Refused"
raw_df_updated_3$DID310D[raw_df_updated_3$DID310D == 9999] <- "Don't know"


# For the column DID320: this is a numerical variable. Therefore, we
# aim to impute the mean. Since the codes 5555, 6666, 7777, and 9999 all have very
# different meanings, this time we impute the mean
# amongst rows that fall in the range 4-520 to prevent extreme skew. 
raw_df_updated_3$DID320[is.na(raw_df_updated_3$DID320)] <- round(mean(raw_df_updated_3$DID320[!is.na(raw_df_updated_3$DID320) 
                                                                                              & raw_df_updated_3$DID320 != 5555
                                                                                              & raw_df_updated_3$DID320 != 6666
                                                                                              & raw_df_updated_3$DID320 != 7777
                                                                                              & raw_df_updated_3$DID320 != 9999]))
raw_df_updated_3$DID320[raw_df_updated_3$DID320 == 5555] = 'Never heard of LDL'
raw_df_updated_3$DID320[raw_df_updated_3$DID320 == 6666] = 'Never had cholesterol test'
raw_df_updated_3$DID320[raw_df_updated_3$DID320 == 7777] = 'Refused'
raw_df_updated_3$DID320[raw_df_updated_3$DID320 == 9999] = "Don't know"


# For the column DID330: this is a numerical variable. Therefore, we
# aim to impute the mean. Since the codes 6666, 7777, and 9999 all have very
# different meanings, this time we impute the mean
# amongst rows that fall in the range 6-205 to prevent extreme skew. 
raw_df_updated_3$DID330[is.na(raw_df_updated_3$DID330)] <- round(mean(raw_df_updated_3$DID330[!is.na(raw_df_updated_3$DID330) 
                                                                                              & raw_df_updated_3$DID330 != 6666
                                                                                              & raw_df_updated_3$DID330 != 7777
                                                                                              & raw_df_updated_3$DID330 != 9999]))
raw_df_updated_3$DID330[raw_df_updated_3$DID330 == 6666] = "Provider didn't specify goal"
raw_df_updated_3$DID330[raw_df_updated_3$DID330 == 7777] = 'Refused'
raw_df_updated_3$DID330[raw_df_updated_3$DID330 == 9999] = "Don't know"


# For the column DID341: this is a numerical variable. Therefore, we
# aim to impute the mean. Since the codes 7777 and 9999 have very
# different meanings, this time we impute the mean
# amongst rows that fall in the range 0-34 to prevent extreme skew. 
raw_df_updated_3$DID341[is.na(raw_df_updated_3$DID341)] <- round(mean(raw_df_updated_3$DID341[!is.na(raw_df_updated_3$DID341) 
                                                                                              & raw_df_updated_3$DID341 != 7777
                                                                                              & raw_df_updated_3$DID341 != 9999]))
raw_df_updated_3$DID341[raw_df_updated_3$DID341 == 7777] = 'Refused'
raw_df_updated_3$DID341[raw_df_updated_3$DID341 == 9999] = "Don't know/not sure"


# We must now combine the two columns DID350 and DIQ350U as was done
# with DID260/DIQ260U columns and DID060/DIQ060U columns. We treat this
# as a categorical column since we have to attach a unit of time (otherwise the
# column would be purely numerical and we would impute the mean). 
raw_df_updated_3$DID350[raw_df_updated_3$DID350 == 7777] <- "Refused"
raw_df_updated_3$DID350[raw_df_updated_3$DID350 == 9999] <- "Don't know"
raw_df_updated_3$DID350[is.na(raw_df_updated_3$DID350)] <- "No data"

raw_df_updated_3$DIQ350U <- ifelse(raw_df_updated_3$DIQ350U == 1, "per day",
                                   ifelse(raw_df_updated_3$DIQ350U == 2, "per week",
                                          ifelse(raw_df_updated_3$DIQ350U == 3, "per month", 
                                                 ifelse(raw_df_updated_3$DIQ350U == 4, "per year", "No data")))) 

new_df_4 <- raw_df_updated_3[, 23:24]
new_new_4 <- unite(new_df_4, '', sep=' ',na.rm=TRUE)
new_new_df_4 <- as.data.frame(new_new_4)
raw_df_updated_3$DID350_units <- new_new_df_4
raw_df_updated_4 <- subset(raw_df_updated_3, select=-c(23,24))
raw_df_updated_4$DID350_units[raw_df_updated_4$DID350_units == ''] <- 'No data'


# For the column DIQ360: This is purely categorical, so we add
# a new column with labels.
sum(is.na(raw_df_updated_4$DIQ360))/nrow(raw_df_updated_4) #0.9116, so no imputation of mode (since > 0.5)
raw_df_updated_4$DIQ360[is.na(raw_df_updated_4$DIQ360)] <- "No data"
raw_df_updated_4$DIQ360_label <- ifelse(raw_df_updated_4$DIQ360 == 1, "Less than 1 month",
                                        ifelse(raw_df_updated_4$DIQ360 == 2, "1-12 months",
                                               ifelse(raw_df_updated_4$DIQ360 == 3, "13-24 months",
                                                      ifelse(raw_df_updated_4$DIQ360 == 4, "Greater than 2 years",
                                                             ifelse(raw_df_updated_4$DIQ360 == 5, "Never",
                                                                    ifelse(raw_df_updated_4$DIQ360 == 7, "Refused",
                                                                           ifelse(raw_df_updated_4$DIQ360 == 9, "Don't know", "No data")))))))

# For the column DIQ080: This is purely categorical, so we add
# a new column with labels.
sum(is.na(raw_df_updated_4$DIQ080))/nrow(raw_df_updated_4) #0.9116, so no imputation of mode (since > 0.5)
raw_df_updated_4$DIQ080[is.na(raw_df_updated_4$DIQ080)] <- "No data"
raw_df_updated_4$DIQ080_label <- ifelse(raw_df_updated_4$DIQ080 == 1, "Yes",
                                        ifelse(raw_df_updated_4$DIQ080 == 2, "No",
                                               ifelse(raw_df_updated_4$DIQ080 == 7, "Refused",
                                                      ifelse(raw_df_updated_4$DIQ080 == 9, "Don't know", "No data"))))


# Here, we return a condensed DataFrame with 1518 entries (about 15% of the original dataset)
# to act as a DataFrame with very robust data. In each row, there must be fewer than 17 entries
# that are classified as "No data" (fewer than 42.5% of the data in the row must be real or imputed data,
# per previous qualifications for imputation to occur). However, this DataFrame cannot
# exist in isolation. We must return another DataFrame that can potentially be used for other
# analytical purposes that preserves the majority of the rows in question. 
condensed_df <- raw_df_updated_4[rowSums(raw_df_updated_4 == "No data") < 18,]

# Here is said DataFrame, which has approximately 98% of the original dataset remaining.
majority_df <- raw_df_updated_4[rowSums(raw_df_updated_4 == "No data") < 19,]

#----------------------- DATE-RELATED ADJUSTMENTS -----------------------

# For the DID040 column: also add a column giving the birth year of the patient in 
# both result tables. We will assume this data was collected at the start of 2016. 
majority_df$DID040_date <- rep(2016,nrow(majority_df)) - as.numeric(majority_df$DID040)
condensed_df$DID040_date <- rep(2016,nrow(condensed_df)) - as.numeric(condensed_df$DID040)


# For the DIQ230 column: give the year in which a patient last saw a diabetes specialist
# (this is an approximate guess and does not have exact values)
majority_df$DIQ230_date <- ifelse(majority_df$DIQ230 == 1, "Within 2015",
                               ifelse(majority_df$DIQ230 == 2, "Between 2014 and 2015",
                               ifelse(majority_df$DIQ230 == 3, "Between 2011 and 2014",
                               ifelse(majority_df$DIQ230 == 4, "Earlier than 2011",
                              ifelse(majority_df$DIQ230 == 5, "Never",
                              ifelse(majority_df$DIQ230 == 7, "Refused",
                               ifelse(majority_df$DIQ230 == 9, "Don't know", "No data")))))))
condensed_df$DIQ230_date <- ifelse(condensed_df$DIQ230 == 1, "Within 2015",
                                  ifelse(condensed_df$DIQ230 == 2, "Between 2014 and 2015",
                                  ifelse(condensed_df$DIQ230 == 3, "Between 2011 and 2014",
                                   ifelse(condensed_df$DIQ230 == 4, "Earlier than 2011",
                                   ifelse(condensed_df$DIQ230 == 5, "Never",
                                   ifelse(condensed_df$DIQ230 == 7, "Refused",
                                  ifelse(condensed_df$DIQ230 == 9, "Don't know", "No data")))))))

# For the DIQ360 column: give the approximate time in which a patient last had their
# pupils dilated for an exam
majority_df$DIQ360_date <- ifelse(majority_df$DIQ360 == 1, "Within the past month in 2016",
                                  ifelse(majority_df$DIQ360 == 2, "2015",
                                  ifelse(majority_df$DIQ360 == 3, "2014",
                                   ifelse(majority_df$DIQ360 == 4, "Earlier than 2014",
                                   ifelse(majority_df$DIQ360 == 5, "Never",
                                   ifelse(majority_df$DIQ360 == 7, "Refused",
                                   ifelse(majority_df$DIQ360 == 9, "Don't know", "No data")))))))
condensed_df$DIQ360_date <- ifelse(condensed_df$DIQ360 == 1, "Within the past month in 2016",
                                 ifelse(condensed_df$DIQ360 == 2, "2015",
                                 ifelse(condensed_df$DIQ360 == 3, "2014",
                                  ifelse(condensed_df$DIQ360 == 4, "Earlier than 2014",
                                 ifelse(condensed_df$DIQ360 == 5, "Never",
                                  ifelse(condensed_df$DIQ360 == 7, "Refused",
                                  ifelse(condensed_df$DIQ360 == 9, "Don't know", "No data")))))))


# We now re-cbind all of the columns so that they can be easily visible in relation to
# one another (for example, for a column that has a labeled counterpart, it would be useful to
# see the two columns side-by-side). Now, the labels are in the correct order
# relative to the original data frame. 
condensed_df_final <- cbind(SEQN=condensed_df$SEQN, 
                            DIQ010=condensed_df$DIQ010,
                            DIQ010_label=condensed_df$DIQ010_label,
                            DID040=condensed_df$DID040,
                            DID040_date=condensed_df$DID040_date,
                            DIQ160=condensed_df$DIQ160,
                            DIQ160_label=condensed_df$DIQ160_label,
                            DIQ170=condensed_df$DIQ170,
                            DIQ170_label=condensed_df$DIQ170_label,
                            DIQ172=condensed_df$DIQ172,
                            DIQ172_label=condensed_df$DIQ172_label,
                            DIQ175_answers=condensed_df$DIQ175_answers,
                            DIQ180=condensed_df$DIQ180,
                            DIQ180_label=condensed_df$DIQ180_label,
                            DIQ050=condensed_df$DIQ050,
                            DIQ050_label=condensed_df$DIQ050_label,
                            DID060_units=condensed_df$DID060_units,
                            DID060_date=condensed_df$DID060_date,
                            DIQ070=condensed_df$DIQ070,
                            DIQ070_label=condensed_df$DIQ070_label,
                            DIQ230=condensed_df$DIQ230,
                            DIQ230_label=condensed_df$DIQ230_label,
                            DIQ230_date=condensed_df$DIQ230_date,
                            DIQ240=condensed_df$DIQ240,
                            DIQ240_label=condensed_df$DIQ240_label,
                            DID250=condensed_df$DID250,
                            DID260_units=condensed_df$DID260_units,
                            DIQ275=condensed_df$DIQ275,
                            DIQ275_label=condensed_df$DIQ275_label,
                            DIQ280=condensed_df$DIQ280,
                            DIQ291=condensed_df$DIQ291,
                            DIQ291_label=condensed_df$DIQ291_label,
                            DIQ300S=condensed_df$DIQ300S,
                            DIQ300D=condensed_df$DIQ300D,
                            DID310S=condensed_df$DID310S,
                            DID310D=condensed_df$DID310D,
                            DID320=condensed_df$DID320,
                            DID330=condensed_df$DID330,
                            DID341=condensed_df$DID341,
                            DID350_units=condensed_df$DID350_units,
                            DIQ360=condensed_df$DIQ360,
                            DIQ360_label=condensed_df$DIQ360_label,
                            DIQ360_date=condensed_df$DIQ360_date,
                            DIQ080=condensed_df$DIQ080,
                            DIQ080_label=condensed_df$DIQ080_label)
colnames(condensed_df_final)[colnames(condensed_df_final) == 'Var.12'] <- 'DIQ175_answers'
colnames(condensed_df_final)[colnames(condensed_df_final) == 'Var.17'] <- 'DID060_units'
colnames(condensed_df_final)[colnames(condensed_df_final) == 'Var.27'] <- 'DID260_units'
colnames(condensed_df_final)[colnames(condensed_df_final) == 'Var.40'] <- 'DID350_units'

majority_df_final <- cbind(SEQN=majority_df$SEQN, 
                           DIQ010=majority_df$DIQ010,
                           DIQ010_label=majority_df$DIQ010_label,
                           DID040=majority_df$DID040,
                           DID040_date=majority_df$DID040_date,
                           DIQ160=majority_df$DIQ160,
                           DIQ160_label=majority_df$DIQ160_label,
                           DIQ170=majority_df$DIQ170,
                           DIQ170_label=majority_df$DIQ170_label,
                           DIQ172=majority_df$DIQ172,
                           DIQ172_label=majority_df$DIQ172_label,
                           DIQ175_answers=majority_df$DIQ175_answers,
                           DIQ180=majority_df$DIQ180,
                           DIQ180_label=majority_df$DIQ180_label,
                           DIQ050=majority_df$DIQ050,
                           DIQ050_label=majority_df$DIQ050_label,
                           DID060_units=majority_df$DID060_units,
                           DID060_date=majority_df$DID060_date,
                           DIQ070=majority_df$DIQ070,
                           DIQ070_label=majority_df$DIQ070_label,
                           DIQ230=majority_df$DIQ230,
                           DIQ230_label=majority_df$DIQ230_label,
                           DIQ230_date=majority_df$DIQ230_date,
                           DIQ240=majority_df$DIQ240,
                           DIQ240_label=majority_df$DIQ240_label,
                           DID250=majority_df$DID250,
                           DID260_units=majority_df$DID260_units,
                           DIQ275=majority_df$DIQ275,
                           DIQ275_label=majority_df$DIQ275_label,
                           DIQ280=majority_df$DIQ280,
                           DIQ291=majority_df$DIQ291,
                           DIQ291_label=majority_df$DIQ291_label,
                           DIQ300S=majority_df$DIQ300S,
                           DIQ300D=majority_df$DIQ300D,
                           DID310S=majority_df$DID310S,
                           DID310D=majority_df$DID310D,
                           DID320=majority_df$DID320,
                           DID330=majority_df$DID330,
                           DID341=majority_df$DID341,
                           DID350_units=majority_df$DID350_units,
                           DIQ360=majority_df$DIQ360,
                           DIQ360_label=majority_df$DIQ360_label,
                           DIQ360_date=majority_df$DIQ360_date,
                           DIQ080=majority_df$DIQ080,
                           DIQ080_label=majority_df$DIQ080_label)

colnames(majority_df_final)[colnames(majority_df_final) == 'Var.12'] <- 'DIQ175_answers'
colnames(majority_df_final)[colnames(majority_df_final) == 'Var.17'] <- 'DID060_units'
colnames(majority_df_final)[colnames(majority_df_final) == 'Var.27'] <- 'DID260_units'
colnames(majority_df_final)[colnames(majority_df_final) == 'Var.40'] <- 'DID350_units'

View(majority_df_final)
View(condensed_df_final)

# Load the two final DataFrames into a .csv file so that the results are visible and accessible to the TAs (turn in both .csv files)
write.csv(majority_df_final, '.../majority_df_final.csv')
write.csv(condensed_df_final, '.../condensed_df_final.csv')

# This completes the data cleaning! Yay!





















































#------------------------[NEW CELL]-------------------------------------
#Now, we verify the counts as are listed on the website.
# For this part, we use SQLdf in order to write SQL code in R. We use the original_df
# in order to do this part (since it has not been manipulated at all, and the
# counts on the website are in line with the original data frame). 
library(sqldf)
original_df <- read_xpt(".../DIQ_I.XPT")
write.csv(original_df, ".../original_df.csv")
original_df <- read.csv(".../original_df.csv")
#-----------------------------------------------------------------

#for the DIQ010 column
res <- sqldf("SELECT DIQ010, COUNT(*)
             from original_df
             group by DIQ010")
View(res)
#yes, the counts match up! 

#-----------------------------------------------------------------

#for the DID040 column
res <- sqldf("SELECT DID040, COUNT(*)
             from original_df
             group by DID040")
View(res)
#yes, the counts match up! (you can use process of elimination
# to find that the values from 2 to 78 add up to the proper number,
# where we know the values for 80, 666, 777, 999, and missing values
# are correct. to demonstrate: 1+2+2+1+2+1+4+3+2+2+3+2+3+5+1+2+3+6+2
#+5+11+6+8+6+5+21+7+8+7+10+27+7+7+16+8+39+9+17+12+13+35+12+13+20+19+62
#+14+36+17+12+36+14+10+14+19+36+9+10+13+12+32+10+9+9+12+14+4+8+3+7+11
#+1+1+3=833, which matches the sum of values from 2-78 given. we
# can also verify this in SQL as follows: 

res_sum <- sqldf("SELECT DID040, COUNT(*)
                 from original_df
                 where DID040 BETWEEN 2 and 78")
View(res_sum)
#this is verified as 833! )

#-----------------------------------------------------------------

#for the DIQ160 column
res <- sqldf("SELECT DIQ160, COUNT(*)
             from original_df
             group by DIQ160")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ170 column
res <- sqldf("SELECT DIQ170, COUNT(*)
             from original_df
             group by DIQ170")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ172 column
res <- sqldf("SELECT DIQ172, COUNT(*)
             from original_df
             group by DIQ172")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ175A column
res <- sqldf("SELECT DIQ175A, COUNT(*)
             from original_df
             group by DIQ175A")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175B column
res <- sqldf("SELECT DIQ175B, COUNT(*)
             from original_df
             group by DIQ175B")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175C column
res <- sqldf("SELECT DIQ175C, COUNT(*)
             from original_df
             group by DIQ175C")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ175D column
res <- sqldf("SELECT DIQ175D, COUNT(*)
             from original_df
             group by DIQ175D")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175E column
res <- sqldf("SELECT DIQ175E, COUNT(*)
             from original_df
             group by DIQ175E")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175F column
res <- sqldf("SELECT DIQ175F, COUNT(*)
             from original_df
             group by DIQ175F")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ175G column
res <- sqldf("SELECT DIQ175G, COUNT(*)
             from original_df
             group by DIQ175G")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ175H column
res <- sqldf("SELECT DIQ175H, COUNT(*)
             from original_df
             group by DIQ175H")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175I column
res <- sqldf("SELECT DIQ175I, COUNT(*)
             from original_df
             group by DIQ175I")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175J column
res <- sqldf("SELECT DIQ175J, COUNT(*)
             from original_df
             group by DIQ175J")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175K column
res <- sqldf("SELECT DIQ175K, COUNT(*)
             from original_df
             group by DIQ175K")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175L column
res <- sqldf("SELECT DIQ175L, COUNT(*)
             from original_df
             group by DIQ175L")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175M column
res <- sqldf("SELECT DIQ175M, COUNT(*)
             from original_df
             group by DIQ175M")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175N column
res <- sqldf("SELECT DIQ175N, COUNT(*)
             from original_df
             group by DIQ175N")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175O column
res <- sqldf("SELECT DIQ175O, COUNT(*)
             from original_df
             group by DIQ175O")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175P column
res <- sqldf("SELECT DIQ175P, COUNT(*)
             from original_df
             group by DIQ175P")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175Q column
res <- sqldf("SELECT DIQ175Q, COUNT(*)
             from original_df
             group by DIQ175Q")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175R column
res <- sqldf("SELECT DIQ175R, COUNT(*)
             from original_df
             group by DIQ175R")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175S column
res <- sqldf("SELECT DIQ175S, COUNT(*)
             from original_df
             group by DIQ175S")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175T column
res <- sqldf("SELECT DIQ175T, COUNT(*)
             from original_df
             group by DIQ175T")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175U column
res <- sqldf("SELECT DIQ175U, COUNT(*)
             from original_df
             group by DIQ175U")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175V column
res <- sqldf("SELECT DIQ175V, COUNT(*)
             from original_df
             group by DIQ175V")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175W column
res <- sqldf("SELECT DIQ175W, COUNT(*)
             from original_df
             group by DIQ175W")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ175X column
res <- sqldf("SELECT DIQ175X, COUNT(*)
             from original_df
             group by DIQ175X")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ180 column
res <- sqldf("SELECT DIQ180, COUNT(*)
             from original_df
             group by DIQ180")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ050 column
res <- sqldf("SELECT DIQ050, COUNT(*)
             from original_df
             group by DIQ050")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DID060 column
res <- sqldf("SELECT DID060, COUNT(*)
             from original_df
             group by DID060")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 666, 777, and 999, along with missing values,
# to find the sum of all the values in rows w/ value 1-55)

#we verify this in SQL
res_sum <- sqldf("SELECT DID060, COUNT(*)
                 from original_df
                 where DID060 BETWEEN 1 and 55")
View(res_sum)

#-----------------------------------------------------------------

#for the DIQ060U column
res <- sqldf("SELECT DIQ060U, COUNT(*)
             from original_df
             group by DIQ060U")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ070 column
res <- sqldf("SELECT DIQ070, COUNT(*)
             from original_df
             group by DIQ070")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ230 column
res <- sqldf("SELECT DIQ230, COUNT(*)
             from original_df
             group by DIQ230")
View(res)
#yes, the counts match up!


#-----------------------------------------------------------------

#for the DIQ240 column
res <- sqldf("SELECT DIQ240, COUNT(*)
             from original_df
             group by DIQ240")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DID250 column
res <- sqldf("SELECT DID250, COUNT(*)
             from original_df
             group by DID250")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 7777, 9999, along with missing values and 0,
# to find the sum of all the values in rows w/ value 1-60)

#we verify this in SQL
res_sum <- sqldf("SELECT DID250, COUNT(*)
                 from original_df
                 where DID250 BETWEEN 1 and 60")
View(res_sum)

#-----------------------------------------------------------------

#for the DID260 column
res <- sqldf("SELECT DID260, COUNT(*)
             from original_df
             group by DID260")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 777, 999, along with missing values and 0,
# to find the sum of all the values in rows w/ value 1-15)

#we verify this in SQL
res_sum <- sqldf("SELECT DID260, COUNT(*)
                 from original_df
                 where DID260 BETWEEN 1 and 15")
View(res_sum)

#-----------------------------------------------------------------

#for the DIQ260U column
res <- sqldf("SELECT DIQ260U, COUNT(*)
             from original_df
             group by DIQ260U")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ275 column
res <- sqldf("SELECT DIQ275, COUNT(*)
             from original_df
             group by DIQ275")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ280 column
res <- sqldf("SELECT DIQ280, COUNT(*)
             from original_df
             group by DIQ280")
View(res)

#yes, the counts match up! (easily seen by process of elimination
# using categories 777, 999, along with missing values,
# to find the sum of all the values in rows w/ value 2-18.5)

#we verify this in SQL
res_sum <- sqldf("SELECT DIQ280, COUNT(*)
                 from original_df
                 where DIQ280 BETWEEN 2 and 18.5")
View(res_sum)

#-----------------------------------------------------------------

#for the DIQ291 column
res <- sqldf("SELECT DIQ291, COUNT(*)
             from original_df
             group by DIQ291")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ300S column
res <- sqldf("SELECT DIQ300S, COUNT(*)
             from original_df
             group by DIQ300S")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 7777, 9999, along with missing values,
# to find the sum of all the values in rows w/ value 80-201)

#we verify this in SQL
res_sum <- sqldf("SELECT DIQ300S, COUNT(*)
                 from original_df
                 where DIQ300S BETWEEN 80 and 201")
View(res_sum)

#-----------------------------------------------------------------

#for the DIQ300D column
res <- sqldf("SELECT DIQ300D, COUNT(*)
             from original_df
             group by DIQ300D")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 7777, 9999, along with missing values,
# to find the sum of all the values in rows w/ value 17-251)

#we verify this in SQL
res_sum <- sqldf("SELECT DIQ300D, COUNT(*)
                 from original_df
                 where DIQ300D BETWEEN 17 and 251")
View(res_sum)

#-----------------------------------------------------------------

#for the DID310S column
res <- sqldf("SELECT DID310S, COUNT(*)
             from original_df
             group by DID310S")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 6666, 7777, and 9999, along with missing values,
# to find the sum of all the values in rows w/ value 80-175)

#we verify this in SQL
res_sum <- sqldf("SELECT DID310S, COUNT(*)
                 from original_df
                 where DID310S BETWEEN 80 and 175")
View(res_sum)

#-----------------------------------------------------------------

#for the DID310D column
res <- sqldf("SELECT DID310D, COUNT(*)
             from original_df
             group by DID310D")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 6666, 7777, 9999, along with missing values,
# to find the sum of all the values in rows w/ value 18-140)

#we verify this in SQL
res_sum <- sqldf("SELECT DID310D, COUNT(*)
                 from original_df
                 where DID310D BETWEEN 18 and 140")
View(res_sum)

#-----------------------------------------------------------------

#for the DID320 column
res <- sqldf("SELECT DID320, COUNT(*)
             from original_df
             group by DID320")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 5555, 6666, 7777, and 9999, along with missing values,
# to find the sum of all the values in rows w/ value 4-520)

#we verify this in SQL
res_sum <- sqldf("SELECT DID320, COUNT(*)
                 from original_df
                 where DID320 BETWEEN 4 and 520")
View(res_sum)

#-----------------------------------------------------------------

#for the DID330 column
res <- sqldf("SELECT DID330, COUNT(*)
             from original_df
             group by DID330")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 6666, 7777, and 9999, along with missing values,
# to find the sum of all the values in rows w/ value 6-205)

#we verify this in SQL
res_sum <- sqldf("SELECT DID330, COUNT(*)
                 from original_df
                 where DID330 BETWEEN 6 and 205")
View(res_sum)

#-----------------------------------------------------------------

#for the DID341 column
res <- sqldf("SELECT DID341, COUNT(*)
             from original_df
             group by DID341")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 7777 and 9999, along with missing values and 0,
# to find the sum of all the values in rows w/ value 1-34)

#we verify this in SQL
res_sum <- sqldf("SELECT DID341, COUNT(*)
                 from original_df
                 where DID341 BETWEEN 1 and 34")
View(res_sum)

#-----------------------------------------------------------------

#for the DID350 column
res <- sqldf("SELECT DID350, COUNT(*)
             from original_df
             group by DID350")
View(res)
#yes, the counts match up! (easily seen by process of elimination
# using categories 7777 and 9999, along with missing values and 0,
# to find the sum of all the values in rows w/ value 1-20)

#we verify this in SQL
res_sum <- sqldf("SELECT DID350, COUNT(*)
                 from original_df
                 where DID350 BETWEEN 1 and 20")
View(res_sum)

#-----------------------------------------------------------------

#for the DIQ350U column
res <- sqldf("SELECT DIQ350U, COUNT(*)
             from original_df
             group by DIQ350U")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ360 column
res <- sqldf("SELECT DIQ360, COUNT(*)
             from original_df
             group by DIQ360")
View(res)
#yes, the counts match up!

#-----------------------------------------------------------------

#for the DIQ080 column
res <- sqldf("SELECT DIQ080, COUNT(*)
             from original_df
             group by DIQ080")
View(res)
#yes, the counts match up! 

