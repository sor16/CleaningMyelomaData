library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plyr)
library(xlsx)
Sys.setlocale(category="LC_ALL",locale="is_IS")
setwd("~/Dropbox/MergaexliReiknir/CleaningMyelomaData")
source("HelpFunctions.R")
source("cancerClean.R")
setwd("../data")
load("mmcomdat.Rdata")
mmcomdat <- mmcomdat %>% ungroup() %>% select(lopnr,INDATUM,diag,diadat_case,byear,KON,fudeath)
#Fjarlaegjum otharfa ICD koda
notWanted <-  c("O","P","Q","S","T","U","W","V","Y","Z")
mmcomdat <- mmcomdat %>% filter(!(notWanted %>% paste(collapse="|") %>% grepl(diag)))
#Fjarlaegjum kommur úr ICD kodum
mmcomdat$diag=gsub(",","",mmcomdat$diag)

#Geymum krabbameinsgreiningar úr sjúkraskrá
cancerDiagnosis <- mmcomdat %>% filter(grepl("^C|^[1][4-9][0-9]|^[2][0][0-9]",mmcomdat$diag)) %>% distinct(lopnr) %>% arrange(as.integer(lopnr))
canmm <- canmm %>% filter(lopnr %in% cancerDiagnosis$lopnr) %>% arrange(lopnr)
#Finnum fjarlægð á milli unique staka í canmm$lopnr
uniqueDist <- diff(which(!duplicated(canmm$lopnr)))
variablesToDuplicate=c("diadat_case","byear","KON","fudeath")
correctCancerDiagnosis <- canmm %>% cbind(lapply(variablesToDuplicate, function(i) DuplicateVariables(cancerDiagnosis[i])) %>% data.frame()) %>% mutate(lopnr=as.character(lopnr))
#Skiptum út krappameinsgreiningum úr sjúkraskrá út fyrir greiningum í krabbameinsskrá
mmcomdat <- mmcomdat %>% filter(!grepl("^C|^[1][0-9][0-9]|^[2][0-3][0-9]",mmcomdat$diag)) %>% rbind(correctCancerDiagnosis)
mmcomdat <- mmcomdat %>% filter(!grepl("^$", mmcomdat$diag))

#Baetum vid thremur breytum, follow up (fu), rokbreytuna dead sem lysir hvort sjuklingur do a timabilinu og time from diagnosis og rada sidan eftir diagnosedDate
mmcomdat <- mmcomdat %>% mutate(fu=fudeath-diadat_case,dead=fudeath != "2013-12-31",timeFromDiagnosis = INDATUM-diadat_case) %>% arrange(desc(diadat_case))
#Búum einnig til breytu sem heldur utan um hvada ICD stadal er studst vid fyrir hverja komu sjuklings
mmcomdat <-  mmcomdat %>% mutate(ICD=cut(INDATUM,breaks=as.Date(c("1958-01-01","1969-01-01","1987-01-01","1997-01-01","2014-01-01")),labels=c("ICD7","ICD8","ICD9","ICD10"),right=FALSE) %>% as.character())

#fu > 0, þ.e. greining fyrir dauda, viljum einnig skoda komur allt ad halfu ari eftir greiningu 
mmcomdat <-  mmcomdat %>% filter(fu > 0 & INDATUM - diadat_case <= 365.25/2)


#Buum til aefingargagnasett med ahugaverdum breytum thar sem einn einstaklingur er i hverri linu og radad er i timarod
outData <-  mmcomdat %>% select(lopnr,diadat_case,fudeath,byear,dead,KON) %>% distinct(lopnr)

#Les in ICD gogn og utby thau yfir a regexpr form
ICDStandard=read.xlsx("greiningarkodarprufa.xlsx",sheetName="ICDMatch",stringsAsFactors=FALSE)
ICDCodesTable=read.xlsx("greiningarkodarprufa.xlsx",sheetName="ICDCodes",stringsAsFactors=FALSE)
ICDCodesList=ICDCodesTable %>% lapply(function(i){
    paste("^",gsub("\\.", ",",i) %>% subset(!is.na(i)) %>%  as.character(),"$",sep="") %>% paste(collapse="|")
    }) %>% data.frame(stringsAsFactors=F)
#Extracta einfaldar breytur og síðan þær sem þurfa frekari flokkun
ICDCodesUnique=ICDCodesList %>% select(-contains("flokkun"))
ICDCodesSykur=ICDCodesList %>% select(contains("sykursýki.flokkun"))
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
outData <- outData %>% mutate(Sykursýki.flokkar.time <- do.call(pmin, outData %>% select(Insúlínháð.sykursýki.flokkun.time,Insúlínóháð.sykursýki.flokkun.time,Óþekkt.sykursýki.flokkun.time))) 
outData <- outData %>% select(-contains("sykursýki.flokkun"))


