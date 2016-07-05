t <- Sys.time()
fullCalibrationList <- crossValidation(data=modelData,fu=modelData$fu,event=modelData$dead,k=10,nrRuns=10,ValidationFunction = calibration,FittingFunction=evalCox,by=0.05,time=365,covariates=Covariates)
Sys.time() - t

time= Sys.time()
FullROCData <- crossValidation(data=modelData,fu=modelData$fu,event=modelData$dead,k=10,ValidationFunction=ROC,FittingFunction=evalCox,nrRuns=10,time=365,covariates=Covariates)
Sys.time()-time

t <- Sys.time()
IntegratedBS <- crossValidation(data=modelData,fu=modelData$fu,event=modelData$dead,k=10,ValidationFunction = IBS,FittingFunction=evalCox,nrRuns = 1,covariates=Covariates)
Sys.time() - t

