#R code to query the AACT cloud PostgreSQL database for new studies--------------
setwd("C:/Users//") # set your own wd
install.packages("RPostgreSQL",repos='https://mirrors.dotsrc.org/cran/') #RPostgreSQL is needed 

#connect with your login details (you need to register yourself first on https://aact.ctti-clinicaltrials.org/ )
library(RPostgreSQL)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user=("***"), password="***")

#download the necessary tables (all columns) - preserve table name
#Define path to destination directory:
#Start by downloading the core 'studies' table:-------------

aact_studies <- dbGetQuery(con, "select * from studies ")
write.csv(aact_studies, "aact_studies.csv", sep="")


aact_conditions <- dbGetQuery(con, "select * from conditions ")
write.csv(aact_conditions, "aact_conditions.csv",sep="")


aact_provided_documents <- dbGetQuery(con, "select * from provided_documents ")
write.csv(aact_provided_documents, "aact_provided_documents.csv")


aact_design_outcomes <- dbGetQuery(con, "select * from design_outcomes ")
write.csv(aact_design_outcomes, "aact_design_outcomes.csv")

aact_eligibilities <- dbGetQuery(con, "select * from eligibilities ")
write.csv(aact_eligibilities, "aact_eligibilities.csv")

aact_design_groups <- dbGetQuery(con, "select * from design_groups ")
write.csv(aact_design_groups,"aact_design_groups.csv")

aact_interventions <- dbGetQuery(con, "select * from interventions ")
write.csv(aact_interventions,"aact_interventions.csv")

aact_detailed_descriptions <- dbGetQuery(con, "select * from detailed_descriptions ")
write.csv(aact_detailed_descriptions, "aact_detailed_descriptions.csv")

aact_brief_summaries <- dbGetQuery(con, "select * from brief_summaries ")
write.csv(aact_brief_summaries, "aact_brief_summaries.csv")

aact_documents  <- dbGetQuery(con, "select * from documents ")
write.csv(aact_documents , "aact_documents .csv")

