library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plyr)
library(xlsx)
library(readxl)
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
    })

#Fall sem finnur allar linur i mmcomdat sem passa vid gefinn ICD koda
matchICD <- function(data,patternToAdd){
    data %>% select(lopnr,timeFromDiagnosis,diag)  %>% filter(grepl(pattern = patternToAdd,data$diag))
}
#Fall sem finnur tha greiningu sem er naest myeloma greiningu
LeastTimeFromDiagnosis <- function(data){
    data %>% group_by(lopnr) %>% slice(abs(timeFromDiagnosis) %>% which.min()) %>% ungroup()
}

AddBoolean <- function(Orgdata,outData,variable){
    matchVariable <- Orgdata %>% matchICD(variable) %>% LeastTimeFromDiagnosis() 
    newVar <-  outData$lopnr %in% matchVariable$lopnr
    matchVariable <- matchVariable %>% mutate(lopnrToOrderFrom=outData[newVar,]$lopnr)
    outData <- outData %>% mutate(newVar.time=Inf)
    outData[newVar,"newVar.time"]=matchVariable[with(matchVariable,match(lopnrToOrderFrom,lopnr)),]$timeFromDiagnosis
    VarAndTime=newVar %>% data.frame(outData$newVar.time) 
    names(VarAndTime) <- c(names(variable),paste(names(variable),"time",sep="."))
    VarAndTime
}
outData <- outData %>% cbind(lapply(1:length(ICDCodes),function(i) AddBoolean(mmcomdat,outData,ICDCodes[i])) %>%data.frame())

#Verkefnid er ad a bua til flokkun a sykursyki, filterum fyrst utfra ICD um sykursyki almennt
Sykur <- mmcomdat %>% matchICD(ICDCodes$Sykursýki) %>% LeastTimeFromDiagnosis() 

# AddCategory <- function(outData,subdata,category){
#     newCategory <- subdata %>% matchICD(ICDCodes[[category]])
#     outData <- outData %>% mutate(as.name(category) = lopnr %in% newCategory$lopnr,as.name(paste(category,".time"))=Inf)
# }

#Einstaklingar med insulinohada sykursyki og timeFromDiagnosis
SykurIndependent <- Sykur %>% matchICD(ICDCodes$Insúlínóháð.sykursýki)
outData <- outData %>% mutate(ICDSykurIndependent = lopnr %in% SykurIndependent$lopnr,ICDSykurIndependent.time=Inf)
SykurIndependent <- SykurIndependent %>% mutate(lopnrToOrderFrom=outData[outData$ICDSykurIndependent,]$lopnr)
outData[outData$ICDSykurIndependent,]$ICDSykurIndependent.time=SykurIndependent[with(SykurIndependent,match(lopnrToOrderFrom,lopnr)),]$timeFromDiagnosis

#Einstaklingar med insulinhada sykursyki og timeFromDiagnosis
SykurDependent <- Sykur %>% matchICD(ICDCodes$Insúlínháð.sykursýki)
outData <- outData %>% mutate(ICDSykurDependent = lopnr %in% SykurDependent$lopnr, ICDSykurDependent.time = Inf)
SykurDependent <- SykurDependent %>% mutate(lopnrToOrderFrom=outData[outData$ICDSykurDependent,]$lopnr)
outData[outData$ICDSykurDependent,]$ICDSykurDependent.time=SykurDependent[with(SykurDependent,match(lopnrToOrderFrom,lopnr)),]$timeFromDiagnosis

SykurUnknown <- Sykur %>% matchICD(ICDCodes$Óþekkt.sykursýki)
outData <- outData %>% mutate(ICDSykurUnknown = lopnr %in% SykurUnknown$lopnr, ICDSykurUnknown.time = Inf)
SykurUnknown <- SykurUnknown %>% mutate(lopnrToOrderFrom=outData[outData$ICDSykurUnknown,]$lopnr)
outData[outData$ICDSykurUnknown,]$ICDSykurUnknown.time=SykurUnknown[with(SykurUnknown,match(lopnrToOrderFrom,lopnr)),]$timeFromDiagnosis

#Breytunum skellt saman og mynda thannig thrja flokka um sykursyki, None,Independent og Dependent
outData <- outData %>% unite(col="Sykursyki",ICDSykurIndependent,ICDSykurDependent,ICDSykurUnknown)
outData$Sykursyki <- factor(outData$Sykursyki,
                            levels=c("FALSE_FALSE_FALSE","TRUE_FALSE_FALSE","FALSE_TRUE_FALSE","FALSE_FALSE_TRUE"),
                            labels = c("None", "Independent", "Dependent","Unknown"))

outData <- outData %>% mutate(Sykursyki.time=do.call(pmin, outData %>% select(ICDSykurIndependent.time,ICDSykurDependent.time,ICDSykurUnknown.time))) 
outData <- outData %>% select(-ICDSykurIndependent.time,-ICDSykurDependent.time,-ICDSykurUnknown.time)


