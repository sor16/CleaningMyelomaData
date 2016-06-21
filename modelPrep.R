#Sameina sykursýkisbreytur í eina flokkabreytu
outData <- outData %>% unite_(col="Diabetes.cat",names(ICDCodesList)[grep("Type",names(ICDCodesList))])
outData$Diabetes.cat <- factor(outData$Diabetes.cat,
                                    levels=c("FALSE_FALSE_FALSE","TRUE_FALSE_FALSE","FALSE_TRUE_FALSE","FALSE_FALSE_TRUE"),
                                    labels = c("None", "Dependent", "Independent","Unknown"))
outData <- outData %>% mutate(Diabetes.cat.time <- do.call(pmin, outData %>% select(Diabetes.Mellitus.Type.1.time, Diabetes.Mellitus.Type.2.time, Unknown.Diabetes.Type.time))) 
outData <- outData %>% select(-contains("Type"))
