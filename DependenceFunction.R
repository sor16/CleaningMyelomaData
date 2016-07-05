library(viridis)
method="Beta3"
filename="Beta3"
#Næ í Cox líkanið
setwd("~/Dropbox/MergaexliReiknir/cleaningMyelomaData")
source("CoxFitting.R")
#Finn öll mismunandi pör af breytum 
pairsOfVariables <- combn(namesOfVariables,m=2) %>% as.character()
#Breytur til þess að leidretta fyrir inní líkaninu
leidrettVariables <- c("kon", "age", "diag_year")
noPairs=length(pairsOfVariables)/2
variableDependence <- lapply(1:noPairs,function(i){
        variable1 <- pairsOfVariables[2*i-1]
        variable2 <- pairsOfVariables[2*i]
        formula <- paste("surv_object",paste(c(leidrettVariables,variable1),collapse="+") %>% paste(variable2,sep="*"),sep="~") %>% as.formula()
        coxModel <- coxph(formula,data=modelData) %>% summary()
        nameExtract <- paste(paste(variable1,"TRUE",sep=""),paste(variable2,"TRUE",sep=""),sep=":")
        if(method=="Beta3"){
            data.frame(coef=coxModel$coefficient[nameExtract,2],pval=coxModel$coefficient[nameExtract,5],
                   variable1=variable1,variable2=variable2)
        }else{
            nameExtractCoef <- paste(namesOfVariables,collapse="|") %>% grepl(rownames(coxModel$coefficients))
            data.frame(coef=prod(coxModel$coefficients[nameExtractCoef,2]),pval=coxModel$coefficient[nameExtract,5],
                       variable1=variable1,variable2=variable2)
        }
    }) %>% bind_rows()
#Bæta við none, þ.e. ekki parað við neitt
variableDependence <- variableDependence %>% rbind(lapply(1:(length(namesOfVariables)),function(i){
    formula <- paste("surv_object",paste(c(leidrettVariables,namesOfVariables[i]),collapse="+"),sep="~") %>% as.formula()
        coxModel <- coxph(formula,data=modelData) %>% summary() 
        nameExtract <- paste(namesOfVariables[i],"TRUE",sep="")
        data.frame(coef=coxModel$coefficients[nameExtract,2],pval=coxModel$coefficients[nameExtract,5],
                   variable1=namesOfVariables[i],variable2="None")
    }) %>% bind_rows())

###PLOTS###-------------------------------------------------------------------------------------------------------------
#Bæti við flokkabreytu útfrá stuðlum 
variableDependence <- variableDependence%>% mutate(RiskCoef=cut(coef,breaks=c(0,0.85,1.15,Inf),labels=c("less risk (<0.85)","unchanged","more risk (>1.15)")))

variableDependence <- variableDependence %>% mutate(count=sapply(1:noPairs,function(i){
    variable1 <- pairsOfVariables[2*i-1]
    variable2 <- pairsOfVariables[2*i]
    sum(outData[[variable1]] & outData[[variable2]])
}) %>% c(.,sapply(namesOfVariables,function(i){
    sum(outData[[i]])
    })))
#Bæti við auka spegluðu eintaki til þess að fá samhverfa mynd
DuplicateReverse <- variableDependence %>% dplyr::rename(variable1=variable2,variable2=variable1)
#Axis ticks order decided when factoring variable1 and variable2, wnat None first
ticks <- namesOfVariables %>% unique() %>% sort()
ticks <- c("None",ticks[ticks!="None"])
variableDependence <- variableDependence %>%rbind(DuplicateReverse) %>% mutate_each(funs(factor(., levels=ticks)),matches("variable")) %>% mutate(Interesting=replace(coef, pval>0.05 | count<20, NA))
setwd("~/Dropbox/MergaexliReiknir/data")
save(variableDependence,file="variableDependence.RData")
#Teikna mynd af coef útfrá samfelldum og strjálum skala
plotDependence=list()
plotDependence[[1]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = RiskCoef),na.rm=T) +
    #geom_text(aes(variable1,variable2,label=coef)) + 
    scale_fill_manual(name="Risk",values=c("Sky Blue", "yellow", "red")) +
    theme_classic() + coord_fixed(ratio=1) + ylab("") + xlab("") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
plotDependence[[2]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = coef),na.rm=T) +
    scale_fill_viridis(name="exp(coef)")  + coord_fixed(ratio=1)  + ylab("") + xlab("")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())

    #Bæti við flokkabreytu útfrá p-gildum
    variableDependence <- variableDependence%>% mutate(pvalHypothesis=cut(pval,breaks=c(0,0.05,1),labels=c("not Independent","Inconclusive")))
    #Teikna mynd af pval útfrá samfelldum og strjálum skala
    plotDependence[[3]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = pvalHypothesis),na.rm=T) +
        scale_fill_manual(name="Hypothesis",values=c("red","Sky Blue"))  + coord_fixed(ratio=1) + ylab("") + xlab("") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    plotDependence[[4]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = pval),na.rm=T) +
        scale_fill_viridis(name="p-value", label=comma)  + coord_fixed(ratio=1) + ylab("") + xlab("") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    #With exclusion no text
    plotDependence[[5]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = Interesting),na.rm=T) +
        scale_fill_viridis(name="exp(coef)")  + coord_fixed(ratio=1)  + ylab("") + xlab("")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    #With exclusion
    plotDependence[[6]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = Interesting),na.rm=T) +
        geom_text(aes(label = count),size=3,colour="red") + scale_fill_viridis(name="exp(coef)")  + coord_fixed(ratio=1)  + ylab("") + xlab("")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    #No of lopnr in every group
    plotDependence[[7]] <- ggplot(data=variableDependence,aes(variable1,variable2)) +
        geom_text(aes(label = count),size=3,colour="red") + theme_bw() + coord_fixed(ratio=1)  + ylab("") + xlab("") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    #exp(coef) in every group
    plotDependence[[8]] <- ggplot(data=variableDependence,aes(variable1,variable2)) + geom_tile(aes(fill = Interesting),na.rm=T) +
        geom_text(aes(label = round(Interesting,3)),size=2.5,colour="red") + scale_fill_viridis(name="exp(coef)")  + coord_fixed(ratio=1)  + ylab("") + xlab("")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    
setwd("~/Dropbox/MergaexliReiknir/Myndir/variableDependence")
names(plotDependence)=c("Risk","coef","Hypothesis","Pvalue","WithExclusion","ExclusionAndText","NoInGroups","CoefsExp")
setwd("~/Dropbox/MergaexliReiknir/Myndir/variableDependence")
pdf(file=paste(filename,"pdf",sep="."), width =16,height = 9.6)
plotDependence[1:length(plotDependence)]
dev.off()

