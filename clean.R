##Hreinsun SR/ISS
# ---- Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(lubridate)

# ---- LoadingFiles ----
setwd("~/Dropbox/MergaexliReiknir/CleaningMyelomaData")
source("HelpFunctions.R")
setwd("../data")
load("mmspmdatvs1krufnExl.RData")
load("mmcomdat.Rdata")

# ---- PreliminaryCleaning ----
#Choosing variables, discarding those which are not important:
mmcomdat <- mmcomdat %>% ungroup() %>% select(lopnr,INDATUM,diag,diadat_case,byear,KON,fudeath) %>% dplyr::rename(kon=KON)

#Removing unneccessary ICD-codes: 
notWanted <-  c("O","P","Q","S","T","U","W","V","Y","Z")
mmcomdat <- mmcomdat %>% filter(!(notWanted %>% paste(collapse="|") %>% grepl(diag)))

#Removing those who are likely to be lost to follow-up (exceeding 30 years of survival after MM diagnosis).
mmcomdat <- mmcomdat %>% filter(fudeath-diadat_case < 30*365.25 | fudeath != "2013-12-31")

#Remove everyone diagnosed before January 1 1973:
mmcomdat <- mmcomdat %>% filter(diadat_case >="1973-01-01")

# ---- AddingCancer ----
#Skiptum út krabbameinsgreiningum úr sjúkraskrá út fyrir greiningum í krabbameinsskrá
canData <- mmspmdatvs1krufnExl %>% filter(group=="cases" & diadat_case >= "1973-01-01" & fudeath-diadat_case > 0) %>% mutate(Cancer=!is.na(can.pre),INDATUM=diadat_case,diag=NA) %>% select(lopnr,INDATUM,diag,diadat_case,byear,KON,fudeath,Cancer) %>% dplyr::rename(kon=KON)
mmcomdat <- mmcomdat %>% filter(!grepl("^C|^[1][0-9][0-9]|^[2][0-3][0-9]",mmcomdat$diag)) %>% mutate(Cancer=FALSE)
mmcomdat <- mmcomdat %>% rbind(canData)
#mmcomdat <- mmcomdat %>% filter(!grepl("^$", mmcomdat$diag))

#Búum til nokkrar breytur, heildarfollowup (fu), dead (já/nei), tími frá greiningu og röðum upp eftir
#greiningardagsetningu: 
mmcomdat <- mmcomdat %>% mutate(fu=fudeath-diadat_case,dead=fudeath != "2013-12-31",timeFromDiagnosis = INDATUM-diadat_case) %>% arrange(desc(diadat_case))
mmcomdat <- mmcomdat %>% mutate(diag_year=year(diadat_case),age = year(diadat_case)-byear)
mmcomdat <- mmcomdat %>% mutate(era = cut(diadat_case, breaks = c('1973-01-01','1983-01-01','1993-01-01','2003-01-01','2014-01-01') %>%
                                              as.Date(),labels=c("1973-1982","1983-1992","1993-2002","2003-2013")))

#Skodum bara greiningar fyrir myeloma greiningu og einnig bara þá sem greinast fyrir dauða með myeloma. 
mmcomdat <-  mmcomdat %>% filter(fu > 0 & INDATUM - diadat_case <= 0)

#Fjarlaegjum kommur úr ICD kodum
mmcomdat$diag=gsub(",","",mmcomdat$diag)

#Notum nú krabbameinsskrá til að hafa greiningar á krabbameini sem réttastar. 
#Tökum út allar ICD greiningar fyrir illkynja krabbamein sem koma úr sjúkrahúsgögnum og setjum
#inn í staðinn greiningar frá krabbameinsskrá (staðfest krabbamein ss).

#Geymum krabbameinsgreiningar úr sjúkraskrá
# cancerDiagnosis <- mmcomdat %>% filter(grepl("^C|^[1][4-9][0-9]|^[2][0][0-9]",mmcomdat$diag)) %>% distinct(lopnr) %>% arrange(as.integer(lopnr))
# canmm <- canmm %>% filter(lopnr %in% cancerDiagnosis$lopnr) %>% arrange(lopnr)
# #Finnum fjarlægð á milli unique staka í canmm$lopnr
# uniqueDist <- diff(which(!duplicated(canmm$lopnr)))
# variablesToDuplicate=c("diadat_case","byear","kon","fudeath")
# correctCancerDiagnosis <- canmm %>% cbind(lapply(variablesToDuplicate, function(i) DuplicateVariables(cancerDiagnosis[i])) %>% data.frame()) %>% mutate(lopnr=as.character(lopnr))


#Búum einnig til breytu sem heldur utan um hvada ICD stadal er studst vid fyrir hverja komu sjuklings
#mmcomdat <-  mmcomdat %>% mutate(ICD=cut(INDATUM,breaks=as.Date(c("1958-01-01","1969-01-01","1987-01-01","1997-01-01","2014-01-01")),labels=c("ICD7","ICD8","ICD9","ICD10"),right=FALSE) %>% as.character())


#Buum til aefingargagnasett med ahugaverdum breytum thar sem einn einstaklingur er i hverri linu og radad er i timarod
outData <-  mmcomdat %>% select(lopnr,diadat_case,fudeath,byear,dead,kon,era,diag_year,age,fu) %>% distinct(lopnr)
outData <- outData %>% cbind(mmcomdat %>% group_by(lopnr) %>% mutate(Cancer=sum(Cancer)!=0) %>% ungroup() %>% distinct(lopnr) %>% select(Cancer))

#Les in ICD gogn og utby thau yfir a regexpr form
ICDStandard=read_excel("greiningarkodarprufa.xlsx",sheet="ICDMatch")
ICDCodesTable=read_excel("greiningarkodarprufa.xlsx",sheet="ICDCodes",col_types=rep("text",ncol(ICDStandard)))
ICDCodesList=ICDCodesTable %>% lapply(function(i){
    paste("^",gsub("\\.", ",",i) %>% subset(!is.na(i)) %>%  as.character(),"$",sep="") %>% paste(collapse="|")
    }) %>% data.frame(stringsAsFactors=F)
#Extracta einfaldar breytur og síðan þær sem þurfa frekari flokkun
ICDCodesUnique=ICDCodesList %>% select(-contains("flokkun"))
ICDCodesSykur=ICDCodesList %>% select(contains("sykursýki.flokkun"))
#ICDCodesHTN=ICDCodes %>% select(contains("HTN"))
mmcomdat <- mmcomdat %>% mutate(diag=ifelse(grepl(ICDCodesList$Nýrnasjúkdómur.TOTAL,mmcomdat$diag) & mmcomdat$timeFromDiagnosis > -365.25/2,NA,mmcomdat$diag))

#Bæti við rokbreytu fyrir comorbitities
outData <- outData %>% cbind(lapply(1:ncol(ICDCodesUnique),function(i) AddBooleanDisease(mmcomdat,outData,ICDCodesUnique[i])) %>%data.frame(stringsAsFactors=F))

#Bæti við flokkun á sykursýki
outData <- outData %>% cbind(lapply(1:ncol(ICDCodesSykur),function(i){
                                mmcomdat %>% matchICD(ICDCodesUnique$Sykursýki.) %>% LeastTimeFromDiagnosis() %>% AddBooleanDisease(outData=outData,variable=ICDCodesSykur[i]) %>% data.frame(stringsAsFactors=F)
                                }))
#Bæta við fjölda af comorbidities
comorbCount <- outData %>% select(matches(paste("^",names(ICDCodesUnique),"$",collapse="|",sep=""))) %>% rowSums()
outData <- outData %>% mutate(comorbCount=comorbCount)
setwd("~/Dropbox/MergaexliReiknir/cleaningMyelomaData")
source("modelPrep.R")


#Save outData to disk
setwd("~/Dropbox/MergaexliReiknir/LikanSamantekt")
save(outData,file="outData.RData")

#outputa gögn inní Shiny App
setwd("~/Dropbox/MergaexliReiknir/RiskUI")
futime <- with(outData,as.numeric(fudeath-diadat_case))
ShinyData <- outData %>% filter(era=="2003-2013")
save(ShinyData,file="ShinyData.RData")

