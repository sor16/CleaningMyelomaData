#Næ í Cox líkan
source("CoxFitting.R")
####Checking what's affecting era variable
eraVariables=c(1,namesOfVariables)
eraEffect <- lapply(1:length(eraVariables),function(i){
    formula <- paste("surv_object ~ era",paste(eraVariables[i],collapse=" + "),sep="+") %>% as.formula()
    outMod <- exp(coxph(formula,data=modelData)$coefficients) %>% t() %>% data.frame() %>% select(matches("era")) %>% mutate(AddOn=eraVariables[i])
    names(outMod)=c("era2","era3","era4","AddOn")
    outMod
}) 
eraEffect <- do.call(rbind,eraEffect)
eraEffect <- melt(eraEffect,id=c("AddOn"))
eraPlot <- ggplot(data=eraEffect,aes(AddOn,value,color=variable)) +geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

tableOfComorb <- lapply(namesOfVariables,function(i){
    table(modelData$era,modelData[[i]]) %>% as.data.frame.matrix() %>% mutate(comorb=i,era=c("era1","era2","era3","era4"))
}) %>% rbind_all()
names(tableOfComorb)=c("FALSE","count","comorb","era")
countPlot <- ggplot(data=tableOfComorb,aes(comorb,count,color=era)) +geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
