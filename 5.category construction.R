#final writing  -----------------
setwd("C:/.../...")

search_results <- read_csv("search_results.csv")
studies_selected<- read_csv("studies_selected_all.csv")
studies_selected_all<- studies_selected
restricted <- data.frame(studies_selected_all$nct_id,studies_selected_all$completion_month_year, studies_selected_all$study_type, studies_selected_all$official_title, studies_selected_all$overall_status, studies_selected_all$phase, studies_selected_all$enrollment, studies_selected_all$source, studies_selected_all$why_stopped)
names(restricted) <- c("nct_id","completion_month_year", "study_type","official_title", "overall_status","phase","enrollment","source", "why_stopped")
search_results_complete <- merge(restricted,search_results, by="nct_id")
search_results_comp <- search_results_complete

# category construction  --------------
library(readr)
search_results_comp$cate <- NA
search_results_comp$a <- NA
search_results_comp$b <- NA
i<-0
for (i in 1:nrow(search_results_comp)) {
  search_results_comp$a[i] <- sum(search_results_comp$sg_detailed[i],
                                  search_results_comp$SG_brief[i], 
                                  search_results_comp$SG_official_title[i],
                                  search_results_comp$SG_brief_title[i],
                                  search_results_comp$SG_outcomes_measure[i],
                                  search_results_comp$SG_new_design_description[i]
                                  ,search_results_comp$SG_groups_title[i]
                                  ,search_results_comp$SG_interventions_description[i]
                                  ,na.rm=TRUE)
}

for (i in 1:nrow(search_results_comp)) {
  search_results_comp$b[i] <- sum(search_results_comp$SG_gender_population[i],
                                  search_results_comp$SG_gender_criteria[i]
                                  ,na.rm=TRUE)
}



search_results_comp$cate[which(search_results_comp$a>0)]<- "a"
search_results_comp$cate[which(search_results_comp$a<1 & search_results_comp$b>0)]<- "b"
search_results_comp$cate[which(search_results_comp$a==0 &search_results_comp$b==0)]<- "c"

search_results_comp$hpv_mention <- c(0)
for (i in 1:nrow(search_results_comp)) {
  search_results_comp$hpv_mention[i] <- sum(search_results_comp$hpv_detailed[i],
                                            search_results_comp$hpv_brief[i],
                                            search_results_comp$hpv_official_title[i],
                                            search_results_comp$hpv_brief_title[i],
                                            search_results_comp$hpv_outcomes_measure[i],
                                            search_results_comp$hpv_new_design_description[i],
                                            search_results_comp$hpv_groups_title[i],
                                            search_results_comp$hpv_interventions_description[i],
                                            search_results_comp$hpv_gender_population[i],
                                            search_results_comp$hpv_gender_criteria[i]
                                            ,na.rm=TRUE)
}

length(which(search_results_comp$hpv_mention>0)) # 

search_results_comp$study_type_c <- ifelse(search_results_comp$study_type=="Observational"|search_results_comp$study_type=="Observational [Patient Registry]", "Observational /Observational [Patient Registry]",search_results_comp$study_type )
search_results_comp$overall_status_c <- ifelse(search_results_comp$overall_status=="Withdrawn"|search_results_comp$overall_status=="Terminated","Withdrawn/Terminated", search_results_comp$overall_status )
search_results_comp$overall_status_c <- ifelse(search_results_comp$overall_status=="Recruiting"|search_results_comp$overall_status=="Enrolling by invitation", "Recruiting/Enrolling by invitation", search_results_comp$overall_status_c )
search_results_comp$overall_status_c <- ifelse(search_results_comp$overall_status=="Unknown status"|search_results_comp$overall_status=="Suspended", "Unknown status/Suspended", search_results_comp$overall_status_c )
length(which(search_results_comp$hpv_mention>0))
search_results_comp$phase_c <- search_results_comp$phase
search_results_comp$phase_c <- ifelse(search_results_comp$phase=="Early Phase 1"|search_results_comp$phase=="Phase 1","Early Phase 1/Phase1",search_results_comp$phase_c)
search_results_comp$phase_c <- ifelse(search_results_comp$phase=="Phase 2"|search_results_comp$phase=="Phase 2/Phase 3	"|search_results_comp$phase=="Phase 3","Phase 2/Phase 3",search_results_comp$phase_c)
write.csv(search_results_comp, "search_results_comp.csv")
