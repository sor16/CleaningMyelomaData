#data=outData
#surv_object <- with(outData,Surv(time=rep(0,nrow(outData)),time2=as.numeric(fudeath-diadat_case), event=dead))
#wantedColumns <- names(outData)[!grepl("time",names(outData))]
#path <- "~/Dropbox/MergaexliReiknir/KaplanMeier"
#startAt=7
#legend = "top-right",ticks="4x",timeInYears = TRUE
generateKaplanMeier <- function(data,surv_object,wantedColumns,path,startAt=1,...){
    require(survival)
    require(devtools)
    #install_github("sor16/ggKaplanMeyer")
    require(ggKaplanMeyer)
    setwd(path)
    wantedColumns <- paste("^",wantedColumns,"$",sep="") %>% paste(collapse = "|")
    plotData <- data %>% select(matches(wantedColumns))
    lapply(startAt:ncol(plotData),function(i) {
        formula <- as.formula(paste("surv_object",names(plotData)[i],sep="~"))
        fit <- survfit(formula, data=plotData)
        pdf(file=paste(names(plotData)[i],"pdf",sep="."), width =16 , height = 9.6,onefile=FALSE)
        grid.newpage()
        grid.draw(addrisk(gg_KM(fit,...)))
        dev.off()
    })
}

outData$timabil <- outData$diadat_case>'2000-01-01'
outData$greiningarar <- as.POSIXlt(outData$diadat_case)$year + 1900
mod1 <- coxph(surv_object ~ KON + timabil + byear + greiningarar + Sykursýki. + Hjartabilun + Offita +
                HTN. + COPD + Astmi + Heilabilun, data=outData)
summary(mod1)
drop1(mod1, test='Chisq')

modrest <- step(mod1)
summary(modrest)
