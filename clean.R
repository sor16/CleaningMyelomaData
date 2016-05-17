library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plyr)
library(xlsx)
Sys.setlocale(category="LC_ALL",locale="is_IS")
setwd("../data")
load("mmcomdat.Rdata")
#Baetum vid thremur breytum, follow up (fu), rokbreytuna dead sem lysir hvort sjuklingur do a timabilinu og time from diagnosis og rada sidan eftir diagnosedDate
mmcomdat <- mmcomdat %>% ungroup() %>% mutate(fu=fudeath-diadat_case,dead=fudeath != "2013-12-31",timeFromDiagnosis = INDATUM-diadat_case) %>% arrange(desc(diadat_case))
#Fjarlaegjum kommur úr ICD kodum
mmcomdat$diag=gsub(",","",mmcomdat$diag) 
#fu > 0, þ.e. greining fyrir dauda, viljum einnig skoda komur allt ad halfu ari eftir greiningu 
mmcomdat <-  mmcomdat %>% filter(fu > 0 & INDATUM - diadat_case <= 365.25/2)

#Fjarlaegjum otharfa ICD koda
notWanted <-  c("O","P","Q","S","T","U","W","V","Y","Z")
mmcomdat <- mmcomdat %>% filter(!(notWanted %>% paste(collapse="|") %>% grepl(diag)))

#Buum til aefingargagnasett med ahugaverdum breytum og thar sem einn einstaklingur er i hverri linu og radad er i timarod
outData <-  mmcomdat %>% select(lopnr,diadat_case,fudeath,byear,dead,KON) %>% distinct(lopnr)

#Les in ICD gogn og utby thau yfir a regexpr form
ICDCodes=read.xlsx("greiningarkodarprufa.xlsx",sheetIndex=1,stringsAsFactors=FALSE)
ICDCodes=ICDCodes %>% lapply(function(i){
    paste("^",gsub("\\.", ",",i) %>% subset(!is.na(i)) %>%  as.character(),"$",sep="") %>% paste(collapse="|")
    }) %>% data.frame(stringsAsFactors=F)
ICDCodesUnique=ICDCodes %>% select(-contains("flokkun"))
ICDCodesSykur=ICDCodes %>% select(contains("sykursýki.flokkun"))
#ICDCodesHTN=ICDCodes %>% select(contains("HTN"))

outData <- outData %>% cbind(lapply(1:ncol(ICDCodesUnique),function(i) AddBooleanDisease(mmcomdat,outData,ICDCodesUnique[i])) %>%data.frame(stringsAsFactors=F))
outData <- outData %>% cbind(lapply(1:ncol(ICDCodesSykur),function(i){
                                mmcomdat %>% matchICD(ICDCodesUnique$Sykursýki.) %>% LeastTimeFromDiagnosis() %>% AddBooleanDisease(outData=outData,variable=ICDCodesSykur[i]) %>% data.frame(stringsAsFactors=F)
                                }))

#uniteVariables <- function(variables,labels){

outData <- outData %>% unite(col="Sykursýki.flokkar",Insúlínháð.sykursýki.flokkun,Insúlínóháð.sykursýki.flokkun,Óþekkt.sykursýki.flokkun)
outData$Sykursýki.flokkar <- factor(outData$Sykursýki.flokkar,
                            levels=c("FALSE_FALSE_FALSE","TRUE_FALSE_FALSE","FALSE_TRUE_FALSE","FALSE_FALSE_TRUE"),
                            labels = c("None", "Dependent", "Independent","Unknown"))
outData <- outData %>% mutate(Sykursýki.flokkar.time=do.call(pmin, outData %>% select(Insúlínháð.sykursýki.flokkun.time,Insúlínóháð.sykursýki.flokkun.time,Óþekkt.sykursýki.flokkun.time))) 
outData <- outData %>% select(-contains("sykursýki.flokkun"))


