# Data_Wrangling_Project_and_Tasks

## Purpose

The purpose of this repository is to act as a source for all of the code that I have written for QBS 181/QSS 30.09 at Dartmouth College in the Fall of 2020. Through this repository (for which each student in QBS 181/QSS 30.09, including me, has received express permission to create and make public, as part of our final assignment), all of my code and approaches are available. 

## Statement of Permission

**PLEASE NOTE: I have been given express permission and instruction—as a student in Professor Ramesh Yapalparvi’s QBS 181/QSS 30.09 course in the Fall of 2020 at Dartmouth College in Hanover, NH—to make my work public in this GitHub repository.**

## Descriptions of Each Sub-Directory

- “project1”: This is my first homework assignment. For this assignment, we examined various ways of cleaning and re-naming columns in patient datasets strictly using SQL Server Management Studio (SSMS) in Windows, making names much more readable and understandable. We also explored renaming columns based on conditions being met in other relevant columns in the same row. Finally, we explored creating new columns in SQL tables that are based on certain criteria being met in pre-existing columns in the given table. 
- “project2”: This is my second homework assignment. In this assignment, we examined how to use ODBC connection strings in order to pull our SQL data from SSMS into R, and then use packages like RODBC and sqldf in order to wrangle the given data and seamlessly integrate it into R’s suite of functions. We practiced renaming columns in R, using SQL “COUNT” and “GROUP BY” functions in order to obtain counts based on certain groups, and date-time functions in R in order to extract certain result sets that are separated by time. This was all completed by filtering by certain column characteristics being met.  
- “project3”: This is my third and final homework assignment. In this assignment, we explored using new packages like dplyr in order to better work with and wrangle data tables in R (in a similar fashion to how SQL works, but perhaps even more readable in certain cases). We utilized functions like “mutate” and “filter” in order to reshape our data, filter it, and create new columns that are based on calculations on pre-existing columns. We also used visualization tools in order to better understand trends in temporal data (again making use of date-time features in R). Finally, we explored using APIs, like the Twitter/Facebook API, in order to gather data from public webpages or Tweets using API keys. Note that I have replaced all of the key/OAuth tokens simply with “…” for privacy purposes. 
- “midterm_project”: This is my midterm examination assignment. In this assignment, we were given a messy dataset containing information about diabetes patients and various demographics features about these patients. We were tasked with examining the data-related issues we saw in this dataset, which included many missing values (to highly varying degrees depending on the column in question), dates that needed to be made uniform and fixed, and column names that did not make sense and should be made more readable. We had to justify all of the changes made and then actually implement them using a combination of both R and SQL, verifying the counts present on the website using SQL specifically.  
- “final_project”: This is my final examination assignment. As part of this assignment, we were tasked with examining blood pressure data on various patients, looking at 12-week intervals of their averaged blood pressure normalcy indicators (an indicator of 1 for controlled blood pressure and 0 for uncontrolled blood pressure) to see which patients responded effectively (and which did not respond effectively) to 12 weeks of intervention. We also examined various tables from our database covering demographic and text-message information of patients in order to choose only those rows that had the most recent text sent. We completed these tasks in a mixture of both SQL and R, as a culmination to all of the work we’ve done so far as part of this class—tying it all together. Finally, each student was required to deposit all of their code into a public GitHub repository as part of the final examination assignment.
