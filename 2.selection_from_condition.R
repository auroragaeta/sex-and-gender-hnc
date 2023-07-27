#Looking into investigated condition presence in browse_condition trough Mesh terms -----

## cancer ----
setwd("C:/Users/../..")
library(readr)
conditions <- read_csv("aact_conditions.csv")
cancer_word <- c(" neopl"," cancer"," malignan"," tumor"," carcino"," onco")

ca <- paste(cancer_word ,collapse="|")
d <- grep(ca,conditions$downcase_name)
length(conditions$downcase_name[d]) 
conditions$cancer_condition<- c(0)
conditions$cancer_condition[d]<-1


## head and neck ---------
hen_word <- c(" head ", " neck "," mouth","oral cavity","pharyn","laryn"," nose","paranasal","salivar", " uadt ","upper aerodigestive tract","GINGIVA"," otorhinolar", "tongue", "tonsil")
hen_word  <- tolower(hen_word)
ca <- paste(hen_word ,collapse="|")
d <- grep(ca,conditions$downcase_name)
conditions$headneck_condition<- c(0)
conditions$headneck_condition[d]<-1
length(which(conditions$headneck_condition==1)) 

#hpv ---------
hen_word_hpv <- c("oral cavity","pharyn","laryn", "tonsil","base of tongue")
hen_word_hpv  <- tolower(hen_word_hpv)
ca <- paste(hen_word_hpv ,collapse="|")
d <- grep(ca,conditions$downcase_name); length(d) 
conditions$orofar<- c(0)
conditions$orofar[d]<-1
length(which(conditions$orofar==1)) 

## defining attinente as the presence of both in the condition  -----------
conditions$attinente<- ifelse(conditions$cancer_condition==1 & conditions$headneck_condition==1,1,0)
length(which(conditions$attinente==1))
attinenza<- table(conditions$nct_id, conditions$attinente)


hpv_oro <- which(conditions$orofar==1 & conditions$attinente==1)
id_hpv_oro <- conditions[hpv_oro,"nct_id"]
nrow(id_hpv_oro) #3405
write.csv(id_hpv_oro, "id_hpv_oro.csv")

write.csv(attinenza,"attinenza.csv")
attinenza <- read_csv("attinenza.csv")
length(which(attinenza$`1`==1)) # 2798  studi trattando come almeno una condizione il tumore testa collo  vs 2803 

attinenza$totale <-c(0)
attinenza$totale=attinenza$`0`+attinenza$`1`
attinenza$attinenza <- c(0)
attinenza$attinenza <- round(attinenza$`1`*100/attinenza$totale,1)

identifiers <- attinenza$...1[which(attinenza$attinenza == 100)]
length(identifiers)# 1908 vs 1952 

write.csv(identifiers, "identifiers.csv")

length(which(attinenza$attinenza==100))

