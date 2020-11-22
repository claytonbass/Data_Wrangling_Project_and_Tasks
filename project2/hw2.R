#hw2

#q1
library(RODBC)
library(sqldf) 

# Remove credentials for public GitHub repository.
myconn <- odbcConnect("[...]",
                      "[...]",
                      "[...]")
#read in all relevant tables
PhoneCall <- sqlQuery(myconn,"select * from dbo.PhoneCall")
PhoneCall_Encounter <- sqlQuery(myconn, "select * from dbo.PhoneCall_Encounter")
CallDuration <- sqlQuery(myconn, "select * from dbo.CallDuration")
Demographics <- sqlQuery(myconn,"select * from dbo.Demographics")
Conditions <- sqlQuery(myconn,"select * from dbo.Conditions")
Text <- sqlQuery(myconn,"select * from dbo.Text")

#write to CSV for all relevant tables to be able to access files from Mac RNotebook
write.csv(PhoneCall, ".../PhoneCall.csv")
write.csv(PhoneCall_Encounter, ".../PhoneCall_Encounter.csv")
write.csv(CallDuration, ".../CallDuration.csv")
write.csv(Demographics,  ".../Demographics.csv")
write.csv(Conditions,  ".../Conditions.csv")
write.csv(Text,  ".../Text.csv")

PhoneCall_and_Encounter <- sqlQuery(myconn, "select a.*, b.* 
from qbs181.dbo.PhoneCall a
left join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId")
write.csv(PhoneCall_and_Encounter, ".../PhoneCall_and_Encounter.csv")
View(PhoneCall_and_Encounter)
View(PhoneCall_Encounter)

PhoneCall_Encounter$EnrollmentGroup <- ifelse(
  PhoneCall_Encounter$EncounterCode == 125060000, 
  "Clinical Alert", 
  ifelse(PhoneCall_Encounter$EncounterCode == 125060001, 
         "Health Coaching", ifelse(PhoneCall_Encounter$EncounterCode == 125060002,
                                   "Technical Question", ifelse(PhoneCall_Encounter$EncounterCode == 125060003, 
                                                                "Administrative", ifelse(PhoneCall_Encounter$EncounterCode == 125060004, "Other",
                        
                                                                                                                                                          ifelse(PhoneCall_Encounter$EncounterCode == 125060005, "Lack of engagement", 
                                                                                                "NA"))))))

record_count <- sqldf("select EnrollmentGroup, count(*) as num_records
from PhoneCall_Encounter
group by EnrollmentGroup") 
write.csv(record_count, ".../record_count.csv")

#a) USE R CODE ?
a <- sqldf("set a.EnrollmentGroup = 'Clinical Alert' 
from (select b.EncounterCode 
from qbs181.dbo.PhoneCall a
inner join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId) c
where c.EncounterCode = 125060000")
  
#b) 
b <- sqldf("set a.EnrollmentGroup = 'Health Coaching' 
from (select b.EncounterCode 
from qbs181.dbo.PhoneCall a
inner join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId) c
where c.EncounterCode = 125060001")

#c)
c <- sqldf("set a.EnrollmentGroup = 'Technical Question' 
from (select b.EncounterCode 
from qbs181.dbo.PhoneCall a
inner join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId) c
where c.EncounterCode = 125060002")

#d)
d <- sqldf("set a.EnrollmentGroup = 'Administrative' 
           from (select b.EncounterCode 
                 from qbs181.dbo.PhoneCall a
                 inner join qbs181.dbo.PhoneCall_Encounter b
                 on a.tri_CustomerIDEntityReference = b.CustomerId) c
           where c.EncounterCode = 125060003")

#e)
e <- sqldf("set a.EnrollmentGroup = 'Other' 
from (select b.EncounterCode 
from qbs181.dbo.PhoneCall a
inner join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId) c
where c.EncounterCode = 125060004")

#f)
f <- sqldf("set a.EnrollmentGroup = 'Lack of engagement' 
from (select b.EncounterCode 
from qbs181.dbo.PhoneCall a
inner join qbs181.dbo.PhoneCall_Encounter b
on a.tri_CustomerIDEntityReference = b.CustomerId) c
where c.EncounterCode = 125060005")

#q2

record_count <- sqldf("select EncounterCode, count(*)
from dbo.PhoneCall_Encounter
group by EncounterCode")
write.csv(PhoneCall, ".../record_count.csv")

#q3

phonecall_callduration <- sqldf("select a.*,b.* from PhoneCall_Encounter a
inner join CallDuration b
on a.CustomerId = b.tri_CustomerIDEntityReference")
write.csv(PhoneCall, ".../phonecall_callduration.csv")


#q4

# Relabeling new columns for "1-Inbound and 2-Outbound" and for 
# "1-No response, 2-Left voice mail and 3-successful" 
CallDuration <- sqldf("alter table CallDuration add CallOutcomeLabel nvarchar(255)")
CallDuration <- sqldf("alter table CallDuration add CallTypeLabel nvarchar(255)")

CallDuration <- sqldf("set CallOutcomeLabel = 'Inbound' from CallDuration
           where CallType = 1")
CallDuration <- sqldf("set CallOutcomeLabel = 'Outbound' from CallDuration
           where CallType = 2")
CallDuration <- sqldf("set CallTypeLabel = 'No response' from CallDuration
           where CallOutcome = 1")
CallDuration <- sqldf("set CallTypeLabel = 'Left voice mail' from CallDuration
           where CallOutcome = 2")
CallDuration <- sqldf("set CallTypeLabel = 'Successful' from CallDuration
           where CallOutcome = 3")

#Create the relevant column labels requested in the question
phonecall_callduration$CallTypeLabel <- ifelse(phonecall_callduration$CallType == "1", "Inbound", ifelse(phonecall_callduration$CallType == "2", "Outbound", "NA"))

phonecall_callduration$CallOutcomeLabel <- ifelse(phonecall_callduration$CallOutcome == "1", "No response", ifelse(phonecall_callduration$CallOutcome == "2", "Left voice mail", ifelse(phonecall_callduration$CallOutcome == "3", "Successful", "NA")))
write.csv(phonecall_callduration, ".../phonecall_callduration.csv")
sum_call_duration <- sqldf("select EnrollmentGroup, sum(CallDuration) as total_duration 
                          from phonecall_callduration
                           group by EnrollmentGroup")
write.csv(sum_call_duration, ".../sum_call_duration.csv")

#Find the number of records for the different call outcomes and call types (in Parallels, and then export to .csv to be used in Mac)
num_records <- sqldf("select CallTypeLabel, CallOutcomeLabel, count(*) from phonecall_callduration group by CallTypeLabel, CallOutcomeLabel")
write.csv(num_records, ".../num_records.csv")

#join PhoneCall and CallDuration
phonecall_callduration <- 

#find call duration for each of the enrollment groups
call_duration <- sqldf("select EnrollmentGroup, sum(CallDuration)
                       from ... group by EnrollmentGroup")
View(call_duration)

#q5

#merge Demographics, Conditions, and Text tables
demo_cond_textmessages <- sqldf("select a.*, b.*, c.*
                                from Demographics a
                                inner join Conditions b
                                on a.contactid = b.tri_patientid
                                inner join Text c
                                on b.tri_patientid = c.tri_contactid")
demo_cond_textmessages$TextSentDate <- format(demo_cond_textmessages$TextSentDate, '%W')
demo_cond_textmessages$TextSentDate <- strftime(demo_cond_textmessages$TextSentDate, format="%V")
write.csv(demo_cond_textmessages, ".../demo_cond_textmessages.csv")

#USE THE MERGED TABLE? 
count_texts <- sqldf("select SenderName, TextSentDate, count(*) as Num_Texts_Per_Week
                     from demo_cond_textmessages
                     group by TextSentDate, SenderName")
write.csv(count_texts, ".../count_texts.csv")

View(count_texts)
#q6

count_texts_cond <- sqldf("select tri_name, TextSentDate, count(*) as Num_Texts_Per_Week
                          from demo_cond_textmessages
                          group by TextSentDate, tri_name")
write.csv(count_texts_cond, ".../count_texts_cond.csv")

View(count_texts_cond)



#### UPDATE PHONECALL TABLE

# Create the correct column in the PhoneCall_Encounter table first
# (then join later to avoid dimensionality issues)
PhoneCall_Encounter$EnrollmentGroup <- ifelse(PhoneCall_Encounter$EncounterCode == 125060000, "Clinical Alert", ifelse(PhoneCall_Encounter$EncounterCode == 125060001, "Health Coaching", ifelse(PhoneCall_Encounter$EncounterCode == 125060002, "Technical Question", ifelse(PhoneCall_Encounter$EncounterCode == 125060003, "Administrative", ifelse(PhoneCall_Encounter$EncounterCode == 125060004, "Other", ifelse(PhoneCall_Encounter$EncounterCode == 125060005, "Lack of engagement", "NA"))))))

# Subset the PhoneCallEncounter table properly 
PhoneCallEncounter_subset <- PhoneCall_Encounter[, c('CustomerId','EnrollmentGroup')]
colnames(PhoneCallEncounter_subset) <- c("ID","EnrollmentGroup")
colnames(PhoneCall) <- c("ID","CallType","CallDuration","CallOutcome","CallStartTime")

# Now merge the DataFrames PhoneCall and PhoneCallEncounter$EnrollmentGroup
PhoneCall_New <- merge(PhoneCall, PhoneCallEncounter_subset, by="ID",
                       all.x=TRUE)
head(PhoneCall_New, 10)

#
sum_call_duration <- sqldf("select EnrollmentGroup, sum(CallDuration) as total_duration from PhoneCall_New
                           group by EnrollmentGroup")
write.csv(sum_call_duration, ".../sum_call_duration.csv")
View(sum_call_duration)


####

sum_call_duration <- sqldf("select EnrollmentGroup, sum(CallDuration) as total_duration 
                          from PhoneCall_Encounter
                           group by EnrollmentGroup")
write.csv(sum_call_duration, ".../sum_call_duration.csv")


######

#Q5 (again)

# THIS PART IS DONE EXCLUSIVELY IN PARALLELS TO WRITE THE CSV TO
# MAC.
#merge Demographics, Conditions, and Text tables
demo_cond_textmessages <- sqldf("select a.*, b.*, c.*
                                from Demographics a
                                inner join Conditions b
                                on a.contactid = b.tri_patientid
                                inner join Text c
                                on b.tri_patientid = c.tri_contactid")

#Convert to weeks in the TextSentDate column. Per the advice of 
# TA Nebula, I have adopted the strftime function to make it easier to
# get the individual weeks. 
demo_cond_textmessages$TextSentDate <- strftime(demo_cond_textmessages$TextSentDate, format="%V")
demo_cond_textmessages$TextSentDate <- format(demo_cond_textmessages$TextSentDate, "%W")
  
library(lubridate)
demo_cond_textmessages$TextSentDate <- floor(yday(as.POSIXlt(demo_cond_textmessages$TextSentDate)) / 7 + 1)
View(demo_cond_textmessages)
#Write to .csv output for use in Mac in the next code chunk
write.csv(demo_cond_textmessages, ".../demo_cond_textmessages.csv")

# Perform the sqldf merge in R on Parallels
count_texts <- sqldf("select SenderName, TextSentDate as WeekNumber, count(*) as Num_Texts_Per_Week
                     from demo_cond_textmessages
                     group by TextSentDate, SenderName")

# Write the final output for Question 5 to .csv to be used on mac
write.csv(count_texts, ".../count_texts.csv")