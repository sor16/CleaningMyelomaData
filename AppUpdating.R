library(survival)
library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
fullCoxModel <- coxph(formula,data=modelData)
pvals=summary(fullCoxModel)$coefficients[,"Pr(>|z|)"] %>% as.numeric()
SignifNames <- Covariates[pvals<0.05]
covariateData=modelData %>% select(one_of(SignifNames))
formulaChar <- paste("surv_object ~ ",paste(SignifNames,collapse="+"),sep="")
formula <- formulaChar %>% as.formula()
fullCoxModel <- coxph(formula,data=modelData)
baseline <- basehaz(fullCoxModel)
baselineCumHazard <- baseline$hazard
#####Predicting survival
#leidretti fyrir medaltolum
predictData <- do.call(cbind,lapply(covariateData, function(x) x - mean(x))) %>% as.data.frame()
#nota predict.coxph
LinearComb <- predict(fullCoxModel) 
survivalPred=exp(-baselineCumHazard[365]*exp(LinearComb))
#Reikna manually
survRaw=exp(-baselineCumHazard[365]*apply(predictData,1,function(i) exp(sum(i*fullCoxModel$coefficients,na.rm=T))))


#Finn hvar einstaklingur lendir innan lifunardreifingar
data=data.frame(survivalPred=survivalPred)
inputSurv=0.8
g <- ggplot(data=data, aes(x=survivalPred)) +
    geom_histogram(aes(y=..density..),binwidth=.01,colour="black", fill="white") +
    geom_density()
    #geom_segment(aes(x = inputSurv, y = 0, xend = inputSurv, yend = 3.4,col="red"))
t <- ggplot_build(g)
surv = t$data[[2]]$x
density <- t$data[[2]]$density
densityData=data.frame(surv=surv,density=density,Me=factor("Me"))
Me <- densityData %>% slice(which.min(abs(inputSurv-surv)))
g <- ggplot(data=densityData, aes(x=surv,y=density))+geom_path()+
    geom_segment(data=Me,aes(x = surv, y = 0, xend = surv, yend = density,col=Me))+theme_bw()+
    theme(legend.title=element_blank())

#Sendi upplýsingar um líkanið í iOSApp
setwd("~/Dropbox/MyelomaApp/RiskScore")
covariateData %>% colMeans() %>% data.frame() %>% write.table(file="Means.txt",row.names=FALSE,col.names=FALSE)
SignifNames[!(SignifNames %in% leidrettVariables)] %>% data.frame() %>% write.table(file="variables.txt",row.names=FALSE,col.names=FALSE)
fullCoxModel$coefficients %>% data.frame() %>% write.table(file="coefficients.txt",row.names=FALSE,col.names=FALSE)
baselineCumHazard %>% data.frame() %>% write.table(file="baseline.txt",row.names=FALSE,col.names=FALSE)
summary(fullCoxModel)$conf.int[,3] %>% as.numeric() %>% log() %>% write.table(file="lower.txt",row.names=FALSE,col.names=FALSE)
summary(fullCoxModel)$conf.int[,4] %>% as.numeric() %>% log() %>% write.table(file="upper.txt",row.names=FALSE,col.names=FALSE)

#Sendi upplýsingar í ShinyApp
setwd("~/Dropbox/MergaexliReiknir/RiskUI")
variables <- SignifNames[!(SignifNames %in% leidrettVariables)]
save(variables,file="variables.RData")
save(baseline,file="baseline.RData")
Means <- covariateData %>% colMeans() %>% as.numeric()
save(Means,file="Means.RData")
coefficients <- fullCoxModel$coefficients
save(coefficients,file="coefficients.RData")
lower <- summary(fullCoxModel)$conf.int[,3] %>% as.numeric() %>% log()
save(lower,file="lower.RData")
upper <- summary(fullCoxModel)$conf.int[,4] %>% as.numeric() %>% log()
save(upper,file="upper.RData")