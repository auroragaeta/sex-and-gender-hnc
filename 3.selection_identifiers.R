#merging with studies ------------------
library(readr)
identifiers <- read_csv("identifiers.csv")
names(identifiers) <- c(".","nct_id") 

aact_studies <- read_csv("aact_studies.csv")
aact_detailed_descriptions <- read_csv("aact_detailed_descriptions.csv")
aact_brief_summaries <- read_csv("aact_brief_summaries.csv")
aact_design_outcomes <- read_csv("aact_design_outcomes.csv")
aact_design_groups <- read_csv("aact_design_groups.csv")
aact_eligibilities <- read_csv("aact_eligibilities.csv")
aact_interventions <- read_csv("aact_interventions.csv")
aact_provided_documents <- read_csv("aact_provided_documents.csv")
nrow(aact_studies) 

studies_selected <- aact_studies[which(aact_studies$nct_id %in% identifiers$nct_id), ]
nrow(studies_selected)


detailed_descriptions <- aact_detailed_descriptions[which(aact_detailed_descriptions$nct_id %in% identifiers$nct_id), ]  
nrow(detailed_descriptions)

brief_summaries <- aact_brief_summaries[which(aact_brief_summaries$nct_id %in% identifiers$nct_id), ]
nrow(brief_summaries)

design_outcomes <- aact_design_outcomes[which(aact_design_outcomes$nct_id %in% identifiers$nct_id), ]
nrow(design_outcomes)

design_groups  <- aact_design_groups[which(aact_design_groups$nct_id %in% identifiers$nct_id), ]
nrow(design_groups)

eligibilities  <- aact_eligibilities[which(aact_eligibilities$nct_id %in% identifiers$nct_id), ]
nrow(eligibilities)

provided_documents  <- aact_provided_documents[which(aact_provided_documents$nct_id %in% identifiers$nct_id), ]
nrow(provided_documents)

interventions  <- aact_interventions[which(aact_interventions$nct_id %in% identifiers$nct_id), ]
nrow(interventions)



setwd("C:/Users/IEO5813/OneDrive - Istituto Europeo di Oncologia/aurora/1.1 sex and gender finale")

write.csv(studies_selected,"studies_selected_all.csv")
write.csv(detailed_descriptions,"detailed_descriptions_all.csv")
write.csv(brief_summaries,"brief_summaries_all.csv")
write.csv(design_outcomes,"design_outcomes_all.csv")
write.csv(design_groups,"design_groups_all.csv")
write.csv(eligibilities,"eligibilities_all.csv")
write.csv(provided_documents,"provided_documents_all.csv")
write.csv(interventions,"interventions_all.csv")
