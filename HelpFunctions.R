#Fall sem endurritar breytu jafn oft og vigurinn uniqueDist gerir ráð fyrir
DuplicateVariables <- function(variable){
    newVar <- rep(variable[[names(variable)]],times=c(uniqueDist,nrow(canmm)-sum(uniqueDist))) %>% data.frame()
    names(newVar) <- names(variable)
    newVar
}
#Fall sem finnur tha greiningu sem er naest myeloma greiningu
LeastTimeFromDiagnosis <- function(data){
    data %>% group_by(lopnr) %>% slice(abs(timeFromDiagnosis) %>% which.min()) %>% ungroup()
}
#Fall sem finnur allar linur i mmcomdat sem passa vid gefinn ICD koda
matchICD <- function(data,patternToAdd){
    #data <- 
    data %>% select(lopnr,timeFromDiagnosis,diag,ICD)  %>% filter(grepl(pattern = patternToAdd,data$diag))
    # dataICD <- data %>% select(diag,ICD) %>% unite(col="diagICD",diag,ICD) %>% '[['(1)
    # StandardICD <- ICDCodesTable %>% select(matches(names(patternToAdd))) %>% bind_cols(ICDStandard %>% select(matches(names(patternToAdd))))
    # names(StandardICD) = c("diag","ICD")
    # StandardICD <- StandardICD %>% unite(col="diagICD",diag,ICD) %>% '[['(1)
    # data <- data %>% filter(dataICD %in% StandardICD) 
}



#Fall sem bætir við rokbreytu við outData sem gefur til kynna hvort einstaklingur se med sjukdom eda ekki
AddBooleanDisease <- function(Orgdata,outData,variable){
    matchVariable <- Orgdata %>% matchICD(variable) %>% LeastTimeFromDiagnosis() 
    newVar <-  outData$lopnr %in% matchVariable$lopnr
    matchVariable <- matchVariable %>% mutate(lopnrToOrderFrom=outData[newVar,]$lopnr)
    outData <- outData %>% mutate(newVar.time=Inf)
    outData[newVar,"newVar.time"]=matchVariable[with(matchVariable,match(lopnrToOrderFrom,lopnr)),]$timeFromDiagnosis
    VarAndTime=newVar %>% data.frame(outData$newVar.time) 
    names(VarAndTime) <- c(names(variable),paste(names(variable),"time",sep="."))
    VarAndTime
}