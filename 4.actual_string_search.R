# S&G Search----------------------
library(readr)
detailed_descriptions_all <- read_csv("detailed_descriptions_all.csv")
brief_summaries_all <- read_csv("brief_summaries_all.csv")
studies_selected_all <- read_csv("studies_selected_all.csv")
design_outcomes_all <- read_csv("design_outcomes_all.csv")
design_groups_all <- read_csv("design_groups_all.csv")
eligibilities_all <- read_csv("eligibilities_all.csv")
interventions_all <-interventions_all <- read_csv("interventions_all.csv")
provided_documents_all <- read_csv("provided_documents_all.csv")

sex_word <- c(" sex", " gender"," woman "," women "," man "," men "," female ", " females ",
              " male "," males ", " girl "," girls "," boy "," boys ",
              " pregnan", " transg")

sex_word  <- tolower(sex_word)
ca <- paste(sex_word,collapse="|")

hpv_word <- c("Human Papillomavirus","HPV Human Papillomavirus","Alphapapillomaviruses","HPV Human Papillomaviruses","Papillomavirus, Human","Human Papillomavirus", "HPV","Human Papillomaviruses, HPV", "Papillomaviruses, Human", "Human Papillomaviruses")

hpv_word  <-gsub("[[:punct:]]", " ",hpv_word)
hpv_word <- tolower(hpv_word)
pv <- paste(hpv_word,collapse="|")


# SEARCH_RESULTS-----------
identifiers <- read_csv("identifiers.csv")
search_results <- data.frame(identifiers)
names(search_results) <- c(".","nct_id")
search_results$sg_detailed <-NA 

## (sg_detailed) detailed description --------
detailed_descriptions_all$description  <-gsub("[[:punct:]]", " ", detailed_descriptions_all$description)  
detailed_descriptions_all$description  <- tolower(detailed_descriptions_all$description)
detailed_descriptions_all$sg_detailed<- NA 
d <- grep(ca,detailed_descriptions_all$description)
length(d) #55
detailed_descriptions_all$sg_detailed <-0
detailed_descriptions_all$sg_detailed[d]<-1
dd <- table(detailed_descriptions_all$nct_id) # no dupe
a <-which(detailed_descriptions_all$sg_detailed==1)
c <- detailed_descriptions_all$nct_id[a]
search_results$sg_detailed <- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(detailed_descriptions_all$sg_detailed==0)
c <- detailed_descriptions_all$nct_id[a]
search_results$sg_detailed <- ifelse(search_results$nct_id %in% c, 0, search_results$sg_detailed)
table(search_results$sg_detailed)

### hpv------
detailed_descriptions_all$hpv_detailed<- NA 
d <- grep(pv,detailed_descriptions_all$description)
length(d) 
detailed_descriptions_all$hpv_detailed <-0
detailed_descriptions_all$hpv_detailed[d]<-1
a <-which(detailed_descriptions_all$hpv_detailed==1)
c <- detailed_descriptions_all$nct_id[a]
search_results$hpv_detailed<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(detailed_descriptions_all$hpv_detailed==0)
c <- detailed_descriptions_all$nct_id[a]
search_results$hpv_detailed <- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_detailed)


# ( SG_brief)  brief summ----------------------
brief_summaries_all$description <-gsub("[[:punct:]]", " ", brief_summaries_all$description)  
brief_summaries_all$description <- tolower(brief_summaries_all$description)
d <- grep(ca,brief_summaries_all$description)
length(d)
brief_summaries_all$SG_brief<-0
brief_summaries_all$SG_brief[d]<-1
search_results$SG_brief <-NA 
a <-which(brief_summaries_all$SG_brief==1)
c <- brief_summaries_all$nct_id[a]
search_results$SG_brief <- ifelse(search_results$nct_id %in% c, 1, NA)

a <-which(brief_summaries_all$SG_brief==0)
c <- brief_summaries_all$nct_id[a]
search_results$SG_brief <- ifelse(search_results$nct_id %in% c, 0, search_results$SG_brief)

#hpv ------
d <- grep(pv,brief_summaries_all$description)
length(d)
brief_summaries_all$hpv_brief<-0
brief_summaries_all$hpv_brief[d]<-1
search_results$hpv_brief<-NA 
a <-which(brief_summaries_all$hpv_brief==1)
c <- brief_summaries_all$nct_id[a]
search_results$hpv_brief<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(brief_summaries_all$hpv_brief==0)
c <- brief_summaries_all$nct_id[a]
search_results$hpv_brief <- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_brief)
table(search_results$hpv_brief)

# (sg_official,sg_brief_t) , studies ------
studies_selected_all$official_title <- tolower(studies_selected_all$official_title)
studies_selected_all$official_title <-gsub("[[:punct:]]", " ", studies_selected_all$official_title)  
studies_selected_all$brief_title <- tolower(studies_selected_all$brief_title)
studies_selected_all$brief_title  <-gsub("[[:punct:]]", " ", studies_selected_all$brief_title)  
search_results$SG_official_title <- NA
search_results$SG_brief_title <- NA 
d <- grep(ca,studies_selected_all$official_title)
d2 <- grep(ca,studies_selected_all$brief_title)
length(d); length(d2)
search_results$SG_official_title<-0;
search_results$SG_official_title[d]<-1
search_results$SG_brief_title<-0;
search_results$SG_brief_title[d2]<- 1
c <- search_results$nct_id[which(search_results$SG_official_title==1)]
search_results$SG_official_title <- ifelse(search_results$nct_id %in% c, 1, search_results$SG_official_title)
c <- search_results$nct_id[which(search_results$SG_brief_title==1)]
search_results$SG_brief_title <- ifelse(search_results$nct_id %in% c, 1, search_results$SG_brief_title)
table(search_results$SG_official_title)
table(search_results$SG_brief_title)

### hpv-----
studies_selected_all$official_title <-gsub("[[:punct:]]", " ", studies_selected_all$official_title) 
studies_selected_all$official_title <- tolower(studies_selected_all$official_title)
studies_selected_all$brief_title <- tolower(studies_selected_all$brief_title)
studies_selected_all$brief_title  <-gsub("[[:punct:]]", " ", studies_selected_all$brief_title)  
search_results$hpv_official_title <- NA
search_results$hpv_brief_title <- NA 
d <- grep(pv,studies_selected_all$official_title)
d2 <- grep(pv,studies_selected_all$brief_title)
length(d); length(d2)
search_results$hpv_official_title<-0;
search_results$hpv_official_title[d]<-1
search_results$hpv_brief_title<-0;
search_results$hpv_brief_title[d2]<- 1
c <- search_results$nct_id[which(search_results$hpv_official_title==1)]
search_results$hpv_official_title <- ifelse(search_results$nct_id %in% c, 1, search_results$hpv_official_title)
c <- search_results$nct_id[which(search_results$hpv_brief_title==1)]
search_results$hpv_brief_title <- ifelse(search_results$nct_id %in% c, 1, search_results$hpv_brief_title)
table(search_results$hpv_official_title)
table(search_results$hpv_brief_title)

# "SG_outcomes_measure-----
design_outcomes_all$measure  <-gsub("[[:punct:]]", " ", design_outcomes_all$measure) 
design_outcomes_all$measure <- tolower(design_outcomes_all$measure)
d <- grep(ca,design_outcomes_all$measure); length(d)
design_outcomes_all$SG_outcomes_measure <-0;design_outcomes_all$SG_outcomes_measure[d]<-1
SG_outcomes_measure <- table(design_outcomes_all$nct_id, design_outcomes_all$SG_outcomes_measure)
write.csv(SG_outcomes_measure, "SG_outcomes_measure.csv")
SG_outcomes_measure <- read_csv("SG_outcomes_measure.csv")
search_results$SG_outcomes_measure <-NA 
a <-which( SG_outcomes_measure$`1`>0);length(a)
c <- SG_outcomes_measure$...1[a]
search_results$SG_outcomes_measure <- ifelse(search_results$nct_id %in% c, 1, NA)
c <- SG_outcomes_measure$...1[-a]
search_results$SG_outcomes_measure <- ifelse(search_results$nct_id %in% c, 0, search_results$SG_outcomes_measure )
table(search_results$SG_outcomes_measure)

## hpv ------
d <- grep(pv,design_outcomes_all$measure); length(d)
design_outcomes_all$hpv_outcomes_measure <-0;design_outcomes_all$hpv_outcomes_measure[d]<-1
hpv_outcomes_measure <- table(design_outcomes_all$nct_id, design_outcomes_all$hpv_outcomes_measure)
write.csv(hpv_outcomes_measure, "hpv_outcomes_measure.csv")
hpv_outcomes_measure <- read_csv("hpv_outcomes_measure.csv")
search_results$hpv_outcomes_measure <-NA 
a <-which( hpv_outcomes_measure$`1`>0);length(a)
c <- hpv_outcomes_measure$...1[a]
search_results$hpv_outcomes_measure <- ifelse(search_results$nct_id %in% c, 1, NA)
c <- hpv_outcomes_measure$...1[-a]
search_results$hpv_outcomes_measure <- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_outcomes_measure )
table(search_results$hpv_outcomes_measure)

# SG_new_design_description--------
design_outcomes_all$description  <-gsub("[[:punct:]]", " ", design_outcomes_all$description) 
design_outcomes_all$description<- tolower(design_outcomes_all$description)
d <- grep(ca,design_outcomes_all$description); length(d)
design_outcomes_all$SG_new_design_description <-0;
design_outcomes_all$SG_new_design_description[d]<-1
SG_new_design_description<- table(design_outcomes_all$nct_id, design_outcomes_all$SG_new_design_description)
write.csv(SG_new_design_description, "SG_new_design_description.csv")
SG_new_design_description <- read_csv("SG_new_design_description.csv")
search_results$SG_new_design_description <-NA 
a <-which(SG_new_design_description$`1`>0); length(a)
c <- SG_new_design_description$...1[a]
search_results$SG_new_design_description <- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(SG_new_design_description$`1`==0)
c <- SG_new_design_description$...1[a];length(c)
search_results$SG_new_design_description <- ifelse(search_results$nct_id %in% c, 0, search_results$SG_new_design_description)
table(search_results$SG_new_design_description)

### hpv ---------
d <- grep(pv,design_outcomes_all$description); length(d)
design_outcomes_all$hpv_new_design_description <-0;
design_outcomes_all$hpv_new_design_description[d]<-1
hpv_new_design_description<- table(design_outcomes_all$nct_id, design_outcomes_all$hpv_new_design_description)
write.csv(hpv_new_design_description, "hpv_new_design_description.csv")
hpv_new_design_description <- read_csv("hpv_new_design_description.csv")
search_results$hpv_new_design_description <-NA 
a <-which(hpv_new_design_description$`1`>0); length(a)
c <- hpv_new_design_description$...1[a]
search_results$hpv_new_design_description <- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(hpv_new_design_description$`1`==0)
c <- hpv_new_design_description$...1[a];length(c)
search_results$hpv_new_design_description <- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_new_design_description)
table(search_results$hpv_new_design_description)


## SG_groups_title,-----
design_groups_all$title<-tolower(design_groups_all$title)
design_groups_all$title  <-gsub("[[:punct:]]", " ", design_groups_all$title) 
d <- grep( ca,design_groups_all$title); length(d)
design_groups_all$SG_groups_title <-0
design_groups_all$SG_groups_title[d]<-1
tit_gr <- table(design_groups_all$nct_id, design_groups_all$SG_groups_title)
write.csv(tit_gr,"tit_gr.csv")
tit_gr<- read_csv("tit_gr.csv")
a <-which(tit_gr$`0`<1)
c <- tit_gr$...1[a]
search_results$SG_groups_title <- NA 
search_results$SG_groups_title<- ifelse(search_results$nct_id %in% c, 1, NA)
c <- tit_gr$...1[which(tit_gr$`0`>0)]
search_results$SG_groups_title<- ifelse(search_results$nct_id %in% c, 0, search_results$SG_groups_title)
table(search_results$SG_groups_title)

### hpv ----------
design_groups_all$title<-tolower(design_groups_all$title)
design_groups_all$title  <-gsub("[[:punct:]]", " ", design_groups_all$title) 
d <- grep(pv,design_groups_all$title); length(d)
design_groups_all$hpv_groups_title <-0
design_groups_all$hpv_groups_title[d]<-1
tit_gr <- table(design_groups_all$nct_id, design_groups_all$hpv_groups_title)
write.csv(tit_gr,"tit_gr.csv")
tit_gr<- read_csv("tit_gr.csv")
a <-which(tit_gr$`0`<1)
c <- tit_gr$...1[a]
search_results$hpv_groups_title <- NA 
search_results$hpv_groups_title<- ifelse(search_results$nct_id %in% c, 1, NA)
c <- tit_gr$...1[which(tit_gr$`0`>0)]
search_results$hpv_groups_title<- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_groups_title)
table(search_results$hpv_groups_title)

# SG_interventions_description (interventon) ----
interventions_all$description <- tolower(interventions_all$description)
interventions_all$description   <-gsub("[[:punct:]]", " ", interventions_all$description )
d <- grep(ca,interventions_all$description); length(d)
interventions_all$SG_interventions_description <-0
interventions_all$SG_interventions_description[d]<-1
int <- table(interventions_all$nct_id, interventions_all$SG_interventions_description)
write.csv(int,"SG_interventions_description.csv")
SG_interventions_description <- read_csv("SG_interventions_description.csv")
search_results$SG_interventions_description<-NA 
a <-which( SG_interventions_description$`1`>0); length(a)
c <- SG_interventions_description$...1[a]
search_results$SG_interventions_description <- ifelse(search_results$nct_id %in% c, 1, NA)
c <- SG_interventions_description$...1[-a]
search_results$SG_interventions_description <- ifelse(search_results$nct_id %in% c, 0, search_results$SG_interventions_description)
table(search_results$SG_interventions_description)

### hpv ---------
interventions_all$description <- tolower(interventions_all$description)
interventions_all$description   <-gsub("[[:punct:]]", " ", interventions_all$description )
d <- grep(pv,interventions_all$description); length(d)
interventions_all$hpv_interventions_description <-0
interventions_all$hpv_interventions_description[d]<-1
int <- table(interventions_all$nct_id, interventions_all$hpv_interventions_description)
write.csv(int,"hpv_interventions_description.csv")
hpv_interventions_description <- read_csv("hpv_interventions_description.csv")
search_results$hpv_interventions_description<-NA 
a <-which( hpv_interventions_description$`1`>0); length(a)
c <- hpv_interventions_description$...1[a]
search_results$hpv_interventions_description <- ifelse(search_results$nct_id %in% c, 1, NA)
c <- hpv_interventions_description$...1[-a]
search_results$hpv_interventions_description <- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_interventions_description)
table(search_results$hpv_interventions_description)

# provided documents----
## sap -------
has_sap <- which(provided_documents_all$has_sap==TRUE) 
sap <- table(provided_documents_all$nct_id,provided_documents_all$has_sap)
write.csv(sap,"has_sap.csv")
c <- provided_documents_all$nct_id[has_sap]
table(provided_documents_all$has_sap) # 42 false, 75 true 
search_results$has_sap <-0
search_results$has_sap <- ifelse(search_results$nct_id %in% c, 1, NA)
has_sap <- read_csv("has_sap.csv")
names(has_sap)<- c("nc","0","1")
search_results$has_sap <- NA 
has_sapp<- which(has_sap$`1`>0)
c <-has_sap$nc[has_sapp]
search_results$has_sap <- ifelse(search_results$nct_id %in% c, 1,search_results$has_sap)
c <-has_sap$nc[-  has_sapp]
search_results$has_sap <- ifelse(search_results$nct_id %in% c, 0
                                 ,search_results$has_sap)

##  provided document -----
provided_documents_all$document_type <- tolower(provided_documents_all$document_type)
provided_documents_all$document_type   <-gsub("[[:punct:]]", " ", provided_documents_all$document_type )
d <- grep("statistical",provided_documents_all$document_type)
length(d)# 75 hanno il piano statistico 
provided_documents_all$stat_paln <-0
provided_documents_all$stat_paln[d]<-1
stat <- table(provided_documents_all$nct_id, provided_documents_all$stat_paln)
write.csv(stat, "stat.csv")
stat <- read_csv("stat.csv")
search_results$SAP_doc<- NA 
a <-which( stat$`1`>0)
c <- stat$...1[a]
search_results$SAP_doc <- ifelse(search_results$nct_id %in% c, 1, NA)
c <- stat$...1[-a]
search_results$SAP_doc <- ifelse(search_results$nct_id %in% c, 0, search_results$SAP_doc)

## SG_gender_popolatione criteria----
eligibilities_all$population  <-gsub("[[:punct:]]", " ", eligibilities_all$population) 
eligibilities_all$population  <- tolower(eligibilities_all$population)
View(eligibilities_all)
d <- grep(ca,eligibilities_all$population  ); length(d)
eligibilities_all$SG_gender_population<-0
eligibilities_all$SG_gender_population[d]<-1
search_results$SG_gender_population <- NA
a <-which(eligibilities_all$SG_gender_population>0)
c <-eligibilities_all$nct_id[a]
search_results$SG_gender_population<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(eligibilities_all$SG_gender_population==0)
c <-eligibilities_all$nct_id[a]
search_results$SG_gender_population<- ifelse(search_results$nct_id %in% c, 0, search_results$SG_gender_population)
table(search_results$SG_gender_population)
eligibilities_all$criteria<- tolower(eligibilities_all$criteria)
eligibilities_all$criteria <-gsub("[[:punct:]]", " ", eligibilities_all$criteria) 
d <- grep(ca,eligibilities_all$criteria ); length(d)
eligibilities_all$SG_gender_criteria<-0
eligibilities_all$SG_gender_criteria[d]<-1
search_results$SG_gender_criteria <- NA
a <-which(eligibilities_all$SG_gender_criteria>0)
c <-eligibilities_all$nct_id[a]
search_results$SG_gender_criteria<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(eligibilities_all$SG_gender_criteria==0)
c <-eligibilities_all$nct_id[a]
search_results$SG_gender_criteria<- ifelse(search_results$nct_id %in% c, 0, search_results$SG_gender_criteria)
table(search_results$SG_gender_criteria)

###hpv-------
eligibilities_all$population  <- tolower(eligibilities_all$population)
eligibilities_all$population  <-gsub("[[:punct:]]", " ", eligibilities_all$population) 
d <- grep(pv,eligibilities_all$population  ); length(d)
eligibilities_all$hpv_gender_population<-0
eligibilities_all$hpv_gender_population[d]<-1
search_results$hpv_gender_population <- NA
a <-which(eligibilities_all$hpv_gender_population>0)
c <-eligibilities_all$nct_id[a]
search_results$hpv_gender_population<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(eligibilities_all$hpv_gender_population==0)
c <-eligibilities_all$nct_id[a]
search_results$hpv_gender_population<- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_gender_population)
table(search_results$hpv_gender_population)
eligibilities_all$criteria<- tolower(eligibilities_all$criteria)
eligibilities_all$criteria <-gsub("[[:punct:]]", " ", eligibilities_all$criteria) 
d <- grep(pv,eligibilities_all$criteria ); length(d)
eligibilities_all$hpv_gender_criteria<-0
eligibilities_all$hpv_gender_criteria[d]<-1
search_results$hpv_gender_criteria <- NA
a <-which(eligibilities_all$hpv_gender_criteria>0)
c <-eligibilities_all$nct_id[a]
search_results$hpv_gender_criteria<- ifelse(search_results$nct_id %in% c, 1, NA)
a <-which(eligibilities_all$hpv_gender_criteria==0)
c <-eligibilities_all$nct_id[a]
search_results$hpv_gender_criteria<- ifelse(search_results$nct_id %in% c, 0, search_results$hpv_gender_criteria)
table(search_results$hpv_gender_criteria)
write.csv(search_results,"search_results.csv")

