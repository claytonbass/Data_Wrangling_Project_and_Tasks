-- load in the dataset

select * from dbo.Demographics
-- verify counts for the DIQ010 column
SELECT DIQ010, COUNT(*)
			from cbass.original_df
			group by DIQ010;
			--yes, the counts match up!

-- verify counts for the DID040 column (pick 10 random rows)
SELECT TOP 10 DID040, COUNT(*)
             from cbass.original_df
             group by DID040
			 order by NEWID()
			 ;
			 -- yes, the counts match up!


-- verify value counts for the same DID040 column
SELECT COUNT(DID040) as num_count
                 from cbass.original_df
                 where try_convert(int,DID040) BETWEEN 2 and 78
				 -- yes, the counts match up!


-- verify counts for the DIQ160 column
SELECT DIQ160, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ160
			 -- yes, the counts match up!


-- verify counts for the DIQ170 column
SELECT DIQ170, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ170
			 -- yes, the counts match up!



-- verify counts for the DIQ172 column
SELECT DIQ172, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ172
			-- yes, the counts match up!

-- verify counts for the DIQ175A column
SELECT DIQ175A, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ175A
			 -- yes, the counts match up!

-- now, skip around to some of the later columns,
-- since we already verified all of these in SQLdf
-- but are simply doing this in SSMS for some of the
-- columns per the instructions

-- verify counts for the DIQ280 column
SELECT TOP 10 DIQ280, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ280
			 order by NEWID();
			 -- yes, the counts match up!

-- verify individual value counts for the same DIQ280 column
SELECT COUNT(DIQ280) as num_count
                 from cbass.original_df
                 where try_convert(float,DIQ280) BETWEEN 2 and 18.5
				 -- yes, the counts match up!

-- verify counts for the DID330 column
SELECT TOP 10 DID330, COUNT(*) as num_counts
             from cbass.original_df
             group by DID330
			 order by NEWID();
			 -- yes, the counts match up!

-- verify individual value counts for the same DID330 column
SELECT COUNT(DID330) as num_count
                 from cbass.original_df
                 where try_convert(int,DID330) BETWEEN 6 and 205;
				 -- yes, the counts match up!



-- verify counts for the DIQ350U column
SELECT DIQ350U, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ350U;

-- verify counts for the DIQ360 column
SELECT DIQ360, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ360;

-- verify counts for the DIQ080 column
SELECT DIQ080, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ080;


-- verify counts for the DIQ291 column
SELECT DIQ291, COUNT(*) as num_count
             from cbass.original_df
             group by DIQ291;

