
setwd("~/Dropbox/mergaexliReiknir/data")
load("variableDependence.RData")
#Finna significant háðar breytur og bæta þeim við líkan
signifVar <- variableDependence %>% filter(pval<0.05 & count>20 & variable1!="None" & variable2!="None") %>% distinct(coef) %>% select(variable1,variable2)
dependentVar <- with(signifVar,paste(paste(variable1,variable2,sep="*"),collapse=" + "))
refitFormula <-  paste(formulaChar,dependentVar,sep=" + ") %>% as.formula()
CoxWithDependence <- coxph(refitFormula,data=modelData)