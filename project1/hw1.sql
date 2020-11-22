select * into cbass.Demographics from dbo.Demographics

--q1 -- I discussed renaming strategies with Professor Yapalparvi, and we discussed this as one of the possible
-- options (although one can also use "AS" aliasing).

-- Also, since question 1 requires us to rename a column to something like "Gender," and question 3 asks us to create 
-- a column "Gender," we drop the "Gender" column from our table (which was suggested by Professor Yapalparvi). 
alter table cbass.Demographics drop column gender
--a)
exec sp_rename 'cbass.Demographics.tri_age','Age','COLUMN';
--b)
exec sp_rename 'cbass.Demographics.gendercode','Gender','COLUMN';
--c)
exec sp_rename 'cbass.Demographics.contactid','ID','COLUMN';
--d)
exec sp_rename 'cbass.Demographics.address1_stateorprovince','State','COLUMN';
--e)
exec sp_rename 'cbass.Demographics.tri_imaginecareenrollmentemailsentdate','EmailSentdate','COLUMN';
--f)
exec sp_rename 'cbass.Demographics.tri_enrollmentcompletedate','Completedate','COLUMN';
--g)
alter table cbass.Demographics add time_to_complete varchar(255);
update cbass.Demographics set time_to_complete = datediff(day, try_convert(date,Emailsentdate), try_convert(date,Completedate))
where Completedate is not null and EmailSentdate is not null;

--additional column changes not explicitly listed
exec sp_rename 'cbass.Demographics.parentcustomeridname','parentcustomerID','COLUMN';
exec sp_rename 'cbass.Demographics.tri_imaginecareenrollmentstatus','TriEnrollmentStatus','COLUMN';

select * from cbass.Demographics;

--q2
alter table cbass.Demographics add EnrollmentStatus nvarchar(255);
--a)
update cbass.Demographics
set EnrollmentStatus = 'Complete' where TriEnrollmentStatus = 167410011;
--b)
update cbass.Demographics
set EnrollmentStatus = 'Email sent' where TriEnrollmentStatus = 167410001;
--c)
update cbass.Demographics
set EnrollmentStatus = 'Non responder' where TriEnrollmentStatus = 167410004;
--d)
update cbass.Demographics
set EnrollmentStatus = 'Facilitated Enrollment' where TriEnrollmentStatus = 167410005;
--e) 
update cbass.Demographics
set EnrollmentStatus = 'Incomplete Enrollments' where TriEnrollmentStatus = 167410002;
--f) 
update cbass.Demographics
set EnrollmentStatus = 'Opted Out' where TriEnrollmentStatus = 167410003;
--g) 
update cbass.Demographics
set EnrollmentStatus = 'Unprocessed' where TriEnrollmentStatus = 167410000;
--h) 
update cbass.Demographics
set EnrollmentStatus = 'Second email sent' where TriEnrollmentStatus = 167410006;

select * from cbass.Demographics;

--q3 -- we add "gender_label" instead of "gender" since we already labeled a column as "gender" in the first question
alter table cbass.Demographics add gender_label nvarchar(255);
--a)
update cbass.Demographics
set gender_label = 'female' where try_convert(int,gender) = 2;
--b)
update cbass.Demographics
set gender_label = 'male' where try_convert(int,gender) = 1;
--c)
update cbass.Demographics
set gender_label = 'other' where try_convert(int,gender) = 167410000;
--d)
update cbass.Demographics
set gender_label = 'Unknown' where try_convert(nvarchar(255),gender) = 'NULL';

select * from cbass.Demographics;


--q4
alter table cbass.Demographics add AgeGroup nvarchar(255);
--a)
update cbass.Demographics
set AgeGroup = '0-25' where age between 0 and 25
--b)
update cbass.Demographics
set AgeGroup = '26-50' where age between 26 and 50
--c)
update cbass.Demographics
set AgeGroup = '51-75' where age between 51 and 75
--d) the maximum age is 87, so our upper age band maxes out at 100
update cbass.Demographics
set AgeGroup = '76-100' where age between 76 and 100

select * from cbass.Demographics;



 

