-- question 2

-- select the Demographics, Conditions, and
-- Text tables
select * into cbass.Demographics from dbo.Demographics
select * into cbass.Conditions from dbo.Conditions
select * into cbass.[Text] from dbo.[Text]

-- merge the three tables into a new table
select a.*, b.*, c.*
into cbass.final_q2
from cbass.Demographics a
inner join cbass.Conditions b
on a.contactid = b.tri_patientid
inner join cbass.[Text] c
on a.contactid = c.tri_contactid;

-- select the rows from the final_q2 table
-- only where the tri_contactid and TextSentDate correspond
-- to the "max" (or latest) TextSentDate per tri_contactid

-- note that it is required that some IDs have two or more
-- rows, since it is possible that multiple texts were sent for two
-- different condition or sender names on the same date (and that date
-- happens to be the latest date that a text was sent)

-- first create a new table where we have only the 
-- maximum/latest dates present
select tri_contactid,  max(TextSentDate) as LatestDate
into cbass.MaxDateTable
from cbass.final_q2
group by tri_contactid;

-- now do an inner join on both the tri_contactid and
-- the TextSentDate so that we get (in most cases)
-- 1 row per tri_contactid (or any of the IDs, for that
-- matter)
select cbass.final_q2.* into cbass.final_q2_answer from cbass.final_q2 
inner join cbass.MaxDateTable 
on cbass.final_q2.tri_contactid = cbass.MaxDateTable.tri_contactid
where TextSentDate = LatestDate;

-- print 10 random rows 
select top 10 * from cbass.final_q2_answer
order by NewID()



