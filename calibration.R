fu=as.numeric(outData$fu)
time=365
event=outData$dead

calibration <- function(fu,time,event,prediction){
    statusAtTime <- fu < time & event
    deciles=seq(0,1,by=0.1)
    sapply(deciles,function(i){
        relevantEntries <- statusAtTime[round(prediction*10)/10 == i]
        length(relevantEntries)
    })

    
}