#IBS
library(pec)
formulaChar <- paste("Surv(as.numeric(fu), event=dead) ~ ",paste(SignifNames,collapse="+"),sep="")
formula <- formulaChar%>% as.formula()
inputModel <- list("Cox.X1"=coxph(formula,data=modelData,y=TRUE))
pec(object=fullCoxModel, formula=formula, data=modelData, times=baseline$time,exact=TRUE)

#Calibration
library(pec)
formulaChar <- paste("Surv(as.numeric(fu), event=dead) ~ ",paste(Covariates,collapse=" + "),sep="")
formula <- formulaChar%>% as.formula()
calPlot(fullCoxModel,time=365,formula=formula,data=modelData)

#ROC
library(survivalROC)
ROCsurvData=modelData
ROCobject=with(ROCsurvData,survivalROC(Stime=as.numeric(fu),status=oneYearStatus,marker=pred,predict.time=365*1,span=0.25*nrow(ROCsurvData)^(-0.20)))
with(ROCobject,plot(FP,TP,type="l"))
abline(c(0,1))