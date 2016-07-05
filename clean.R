##Hreinsun SR/ISS
# ---- Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(lubridate)
library(survival)

# ---- LoadingFiles ----
setwd("~/Dropbox/MergaexliReiknir/cleaningMyelomaData")
homeDir=getwd()
source("HelpFunctions.R")
setwd("~/Dropbox/MergaexliReiknir/data")
load("mmspmdatvs1krufnExl.RData")
load("mmcomdat.Rdata")

setwd(homeDir)

# ---- PreliminaryCleaning ----
#Choosing variables, discarding those which are not important:
mmcomdat <- mmcomdat %>% ungroup() %>% select(lopnr,INDATUM,diag,diadat_case,byear,KON,fudeath) %>% dplyr::rename(kon=KON)

#Removing unneccessary ICD-codes: 
notWanted <-  c("O","P","Q","S","T","U","W","V","Y","Z")
mmcomdat <- mmcomdat %>% filter(!(notWanted %>% paste(collapse="|") %>% grepl(diag)))

#Removing those who are likely to be lost to follow-up (exceeding 30 years of survival after MM diagnosis).
mmcomdat <- mmcomdat %>% filter(fudeath-diadat_case < 30*365.25 | fudeath != "2013-12-31")

#Remove everyone diagnosed before January 1 1973:
mmcomdat <- mmcomdat %>% filter(diadat_case >="1973-01-01" & INDATUM >= "1970-01-01")

# ---- AddingCancer ----
canData <- mmspmdatvs1krufnExl %>% 
    filter(group=="cases" & diadat_case >= "1973-01-01" & fudeath-diadat_case > 0) %>%
    mutate(Cancer=!is.na(can.pre),INDATUM=diadat_case,diag=NA) %>% 
    select(lopnr,INDATUM,diag,diadat_case,byear,KON,fudeath,Cancer) %>% 
    dplyr::rename(kon=KON)
mmcomdat <- mmcomdat %>% filter(!grepl("^C|^[1][0-9][0-9]|^[2][0-3][0-9]",mmcomdat$diag)) %>% 
    mutate(Cancer=FALSE)
mmcomdat <- mmcomdat %>% rbind(canData)

# ---- newVariables ----
mmcomdat <- mmcomdat %>% mutate(fu=fudeath-diadat_case)
mmcomdat <- mmcomdat %>% mutate(dead=fudeath != "2013-12-31") 
mmcomdat <- mmcomdat %>% mutate(timeFromDiagnosis = INDATUM-diadat_case) %>% arrange(desc(diadat_case))
mmcomdat <- mmcomdat %>% mutate(diag_year=year(diadat_case))
mmcomdat <- mmcomdat %>% mutate(age = year(diadat_case)-byear)
mmcomdat <- mmcomdat %>% mutate(era = cut(diadat_case, breaks = c('1973-01-01','1983-01-01','1993-01-01','2003-01-01','2014-01-01') %>% as.Date(),labels=c("1973-1982","1983-1992","1993-2002","2003-2013")))
# ---- exclusion ----
mmcomdat <-  mmcomdat %>% filter(fu > 0 & INDATUM - diadat_case <= 0)
mmcomdat$diag <- gsub(",","",mmcomdat$diag)
# ---- ExcelInput ----
#Reading ICD codes from Excel file
setwd("~/Dropbox/MergaexliReiknir/data")
ICDStandard <- read_excel("greiningarkodarprufa.xlsx",sheet="NewMatch")
ICDCodesTable <- read_excel("greiningarkodarprufa.xlsx",sheet="New",col_types=rep("text",ncol(ICDStandard)))
ICDCodesList <- ICDCodesTable %>% lapply(function(i){
    paste("^",gsub("\\.", ",",i) %>% subset(!is.na(i)) %>%  as.character(),"$",sep="") %>% paste(collapse="|")
}) %>% data.frame(stringsAsFactors=F)
setwd(homeDir)

# ---- renalDisease ----
#For chronic kidney disease, the diagnosis has to be at least 6 months prior to MM diagnosis 
mmcomdat <- mmcomdat %>% 
    mutate(diag=ifelse(grepl(ICDCodesList$Chronic.renal.disease,mmcomdat$diag) & mmcomdat$timeFromDiagnosis > -365.25/2,NA,mmcomdat$diag))

# ---- createOutData ----
#Creating OutData with distinct lopnr, starting with cancer diagnosis which is cross-linked with another data set: 
outData <-  mmcomdat %>% select(lopnr,diadat_case,fudeath,byear,dead,kon,era,diag_year,age,fu) %>% distinct(lopnr,.keep_all=TRUE)
outData <- outData %>% cbind(mmcomdat %>% group_by(lopnr) %>% 
                                 mutate(Cancer=sum(Cancer)!=0) %>% 
                                 ungroup() %>% 
                                 distinct(lopnr,.keep_all=TRUE) %>% 
                                 select(Cancer))

#Adding boolean variables for comorbitities:
outData <- outData %>% cbind(lapply(1:ncol(ICDCodesList),function(i) AddBooleanDisease(mmcomdat,outData,ICDCodesList[i])) %>%data.frame(stringsAsFactors=F))

# ---- factoring HTN and Diabetes ----
outData <- outData %>% mutate(Diabetes.Mellitus=replace(Diabetes.Mellitus,Diabetes.Mellitus==TRUE & Diabetes.end.organ.diagroup,FALSE))
outData <- outData %>% mutate(Hypertension=replace(Hypertension,Hypertension==TRUE & Hypertension.with.end.organ.disease,FALSE))
# groups <- c("Diabetes","Hypertension")
# newFactors <- lapply(groups,function(i){
#     groupData <- outData %>% select(contains(i),-contains("time"))
#     EndOrg <- groupData %>% select(contains("end.organ")) %>% unlist() %>% as.numeric() %>% replace(.,.==TRUE,2) 
#     WithoutEndOrg <- groupData %>% select(-contains("end.organ")) %>% unlist() %>% as.numeric() %>% replace(.,.==TRUE & EndOrg,FALSE)
#     data.frame(EndOrg + WithoutEndOrg)
# }) %>% bind_cols()
# names(newFactors) <- groups
# newFactors <- newFactors %>% mutate_each(funs(factor))
# outData <- outData %>% select(-contains("Diabetes"),-contains("Hypertension"))
# outData <- outData %>% cbind(newFactors)

# ---- comorbCount ----

#Creating a new variable containing the number of comorbidities a patient has
comorbCount <- outData %>% select(matches(paste("^",names(ICDCodesList),"$",collapse="|",sep=""))) %>% rowSums()
outData <- outData %>% mutate(comorbCount=comorbCount)

# ---- modelPreperation ----

#Creating survival object
surv_object <- with(outData,Surv(as.numeric(fu), event=dead))
#Correction covariates
leidrettVariables <- c("diag_year","kon", "age")
#remove variables that are not used in model
modelData <- outData %>% select(-contains("time"))
#modelData <- outData %>% select(-contains("cat"),-contains("time"))
#removing diseases not of interest
#modelData <- modelData %>% select(-matches("Autoimmune.disease..antibody.positive|Autoimmune.disease..antibody.negative|HIV.AIDS"))
namesOfVariables <- names(modelData)[grep("^[A-Z]",names(modelData))]
Covariates=c(leidrettVariables,namesOfVariables)
modelData <- modelData %>% select(one_of(c("fu","dead",leidrettVariables,namesOfVariables)))
formulaChar <- paste("surv_object ~ ",paste(Covariates,collapse=" + "),sep="")
formula <- formulaChar %>% as.formula()
setwd("~/Dropbox/MergaexliReiknir/data")
save(modelData,file="modelData.RData")
setwd(homeDir)
# ---- savingToDisk ----
#Save outData to disk
setwd("~/Dropbox/MergaexliReiknir/LikanSamantekt")
save(outData,file="outData.RData")

#outputa gögn inní Shiny App
setwd("~/Dropbox/MergaexliReiknir/RiskUI")
futime <- with(outData,as.numeric(fudeath-diadat_case))
ShinyData <- outData %>% filter(era=="2003-2013")
save(ShinyData,file="ShinyData.RData")
setwd(homeDir)

