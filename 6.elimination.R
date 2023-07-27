#exclusion of Withdrawn and Terminated----------
library(readr)
setwd("C:/.../...")

search_results_comp <- read_csv("search_results_comp.csv")

cancellati <- search_results_comp$nct_id[which(search_results_comp$overall_status_c=="Withdrawn/Terminated")]
length(cancellati)
table(search_results_comp$hpv_mention[which(search_results_comp$overall_status_c=="Withdrawn/Terminated")])

search_results_finale <- search_results_comp[-which( search_results_comp$nct_id %in% cancellati),]
nrow(search_results_finale) 
length(which( search_results_comp$nct_id %in% cancellati))
which( search_results_comp$nct_id %in% cancellati)
nrow(search_results_finale)


#exclusion of single sex studies ----------------
eligibilities <- read_csv("eligibilities_all.csv")
single_sex <- eligibilities$nct_id[which(eligibilities$gender=="Male"|eligibilities$gender=="Female")]
length(single_sex)#8
single_sex_search<- which(search_results_finale$nct_id %in% single_sex)
search_results_finale <- search_results_finale[-which(search_results_finale$nct_id %in% single_sex),]
nrow(search_results_finale)


search_results_finale$hpv01 <- ifelse(search_results_finale$hpv_mention>0,1,0)
id_hpv_oro <- read_csv("id_hpv_oro.csv")
length(which(search_results_finale$nct_id %in% id_hpv_oro$nct_id))

write.csv(search_results_finale, "search_results_FINALE.csv")
nrow(search_results_finale)
