    setwd("~/Dropbox/MergaexliReiknir/Myndir/KaplanMeier/Survival")
    surv_object <- with(outData,Surv(time=rep(0,nrow(outData)),time2=as.numeric(fudeath-diadat_case), event=dead))
    wantedColumns <- names(outData)[!grepl("time|^[a-z]",names(outData)) ]
    wantedColumns <- paste("^",wantedColumns,"$",sep="") %>% paste(collapse = "|")
    plotData <- outData %>% select(matches(wantedColumns))
    #plot Kaplan Meier
    multiKaplanMeier(data=plotData,surv_object,wantedColumns,path="~/Dropbox/MergaexliReiknir/Myndir/KaplanMeier/Survival",legend = "top-right",ticks="4x",cumIncidence=FALSE,timeInYears = TRUE)
    #plot cumIncidence
    multiKaplanMeier(data=plotData,surv_object,wantedColumns,path="~/Dropbox/MergaexliReiknir/Myndir/KaplanMeier/cumulativeIncidence",legend = "top-right",ticks="4x",cumIncidence=TRUE,timeInYears = TRUE)

