library(survival)
library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
#Bý til survival object
surv_object <- with(outData,Surv(as.numeric(fu), event=dead))
#Breytur til þess að leidretta fyrir inní líkaninu
leidrettVariables <- c("diag_year","kon", "age")
#Fjarlægi tímabreytu
modelData <- outData %>% select(-contains("flokkar"),-contains("time"))
#Fjarlægi ómarktækar breytur og fitta Cox
modelData <- modelData %>% select(-matches("Obesity|Hypertension|Astmi|Sjálfsofnæmissjúkd..Mótefnaneikv..|Sjálfsofnæmissjd..Mótefnajákv.|HIV.AIDS"))
namesOfVariables <- names(modelData)[grep("^[A-Z]",names(modelData))]
formulaChar <- paste("surv_object ~ ",paste(leidrettVariables,collapse="+"),"+",paste(namesOfVariables,collapse="+"),sep="")
formula <- formulaChar%>% as.formula()
fullCoxModel <- coxph(formula,data=modelData)
baselineHazard <- basehaz(fullCoxModel)
#Sendi upplýsingar um líkanið í iOSApp
setwd("~/Dropbox/iOSApp/RiskScore/RiskScore")
namesOfVariables %>% data.frame() %>% write.table(file="variables.txt",row.names=FALSE,col.names=FALSE)
fullCoxModel$coefficients %>% data.frame() %>% write.table(file="coefficients.txt",row.names=FALSE,col.names=FALSE)
baselineHazard$hazard %>% data.frame() %>% write.table(file="baseline.txt",row.names=FALSE,col.names=FALSE)

setwd("~/Dropbox/mergaexliReiknir/data")
load("variableDependence.RData")
#Finna significant háðar breytur og bæta þeim við líkan
signifVar <- variableDependence %>% filter(pval<0.05 & count>20 & variable1!="None" & variable2!="None") %>% distinct(coef) %>% select(variable1,variable2)
dependentVar <- with(signifVar,paste(paste(variable1,variable2,sep="*"),collapse=" + "))
refitFormula <-  paste(formulaChar,dependentVar,sep=" + ") %>% as.formula()
CoxWithDependence <- coxph(refitFormula,data=modelData)