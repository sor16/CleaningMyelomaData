library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plyr)
library(xlsx)
Sys.setlocale(category="LC_ALL",locale="is_IS")
setwd(../data)
load("mmcomdat.Rdata")
#Baetum vid thremur breytum, follow up (fu), rokbreytuna dead sem lysir hvort sjuklingur do a timabilinu og time from diagnosis
mmcomdat <- mmcomdat %>% ungroup() %>% mutate(fu=fudeath-diadat_case,dead=fudeath != "2013-12-31",timeFromDiagnosis = INDATUM-diadat_case)

#fu > 0, þ.e. greining fyrir dauda, viljum einnig skoda komur allt ad halfu ari eftir greiningu 
mmcomdat <-  mmcomdat %>% filter(fu > 0 & INDATUM - diadat_case <= 365.25/2)

#Fjarlaegjum otharfa ICD koda
notWanted <-  c("O","P","Q","S","T","U","W","V","Y","Z")
mmcomdat <- mmcomdat %>% filter(!(notWanted %>% paste(collapse="|") %>% grepl(diag)))

#Buum til aefingargagnasett med ahugaverdum breytum og thar sem einn einstaklingur er i hverri linu og radad er i timarod
outData <-  mmcomdat %>% select(lopnr,diadat_case,fudeath,byear,dead,KON) %>% distinct(lopnr) %>% arrange(desc(diadat_case))

#Les in ICD gogn og utby thau yfir a regexpr form
ICDCodes=read.xlsx("greiningarkodar.xlsx",sheetIndex=1)
ICDCodes=ICDCodes %>% lapply(function(i){
    gsub("\\.", ",",i) %>% subset(!is.na(i)) %>%  as.character() %>% paste(collapse="|")
    })

#Fall sem finnur allar linur i mmcomdat sem passa vid gefinn ICD koda
matchICD <- function(data,patternToAdd){
    data %>% select(lopnr,timeFromDiagnosis,diag)  %>% filter(grepl(pattern = patternToAdd,data$diag))
}
#Fall sem finnur tha greiningu sem er naest myeloma greiningu
LeastTimeFromDiagnosis <- function(data){
    data %>% group_by(lopnr) %>% slice(abs(timeFromDiagnosis) %>% which.min()) %>% ungroup()
}

#Dalkur med rokbreytu sem gefur til kynna hvort einstaklingur hafi greinst med hjartasjukdom
ICDKrans <- mmcomdat %>% matchICD(ICDCodes$Kransæðasjúkdómur)
outData <- outData %>% mutate(ICDKrans = lopnr %in% ICDKrans$lopnr)

#Verkefnid er ad a bua til flokkun a sykursyki, filterum fyrst utfra ICD um sykursyki almennt
Sykur <- mmcomdat %>% matchICD(ICDCodes$Sykursýki) %>% LeastTimeFromDiagnosis() 

#Einstaklingar med insulinohada sykursyki og timeFromDiagnosis
SykurIndependent <- Sykur %>% matchICD(ICDCodes$Insúlínóháð.sykursýki)
outData <- outData %>% mutate(ICDSykurIndependent = lopnr %in% SykurIndependent$lopnr,ICDSykurIndependent.time=Inf)
outData[outData$ICDSykurIndependent,]$ICDSykurIndependent.time = SykurIndependent$timeFromDiagnosis

#Einstaklingar med insulinhada sykursyki og timeFromDiagnosis
SykurDependent <- Sykur %>% matchICD(ICDCodes$Insúlínháð.sykursýki)
outData <- outData %>% mutate(ICDSykurDependent = lopnr %in% SykurDependent$lopnr, ICDSykurDependent.time = Inf)
outData[outData$ICDSykurDependent,]$ICDSykurDependent.time = SykurDependent$timeFromDiagnosis

SykurUnknown <- Sykur %>% matchICD(ICDCodes$Óþekkt.sykursýki)
outData <- outData %>% mutate(ICDSykurUnknown = lopnr %in% SykurUnknown$lopnr, ICDSykurUnknown.time = Inf)
outData[outData$ICDSykurUnknown,]$ICDSykurUnknown.time = SykurUnknown$timeFromDiagnosis

#Breytunum skellt saman og mynda thannig thrja flokka um sykursyki, None,Independent og Dependent
outData <- outData %>% unite(col="Sykursyki",ICDSykurIndependent,ICDSykurDependent)
outData$Sykursyki <- factor(outData$Sykursyki,levels=c("FALSE_FALSE","TRUE_FALSE","FALSE_TRUE"), labels = c("None", "Independent", "Dependent"))
outData <- outData %>% mutate(Sykursyki.time=pmin(outData$ICDSykurIndependent.time,outData$ICDSykurDependent.time))
outData <- outData %>% select(-ICDSykurIndependent.time,-ICDSykurDependent.time)


