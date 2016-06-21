library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(xlsx)
Sys.setlocale(category="LC_ALL",locale="is_IS")
setwd("~/Dropbox/MergaexliReiknir/data")
load("canmm.RData")
classChanges <- function(i){
    i <- as.character(i)
    i[is.na(i)] <- ""
    i
}
#Sameining ICD breyta yfir i diag
canmm <- canmm %>% ungroup() %>% mutate_each(funs(classChanges),ICD7,ICD9) %>% gather(ICD,diag,matches("^ICD7$|^ICD9$|^ICDO10$"))
canmm <- canmm  %>% select(lopnr,diadatn,diag) %>% rename(c(diadatn="INDATUM"))

#mmcomdat <- mmcomdat %>% filter(!grepl("^$", mmcomdat$diag))
#Notum nú krabbameinsskrá til að hafa greiningar á krabbameini sem réttastar. 
#Tökum út allar ICD greiningar fyrir illkynja krabbamein sem koma úr sjúkrahúsgögnum og setjum
#inn í staðinn greiningar frá krabbameinsskrá (staðfest krabbamein ss).

#Geymum krabbameinsgreiningar úr sjúkraskrá
# cancerDiagnosis <- mmcomdat %>% filter(grepl("^C|^[1][4-9][0-9]|^[2][0][0-9]",mmcomdat$diag)) %>% distinct(lopnr) %>% arrange(as.integer(lopnr))
# canmm <- canmm %>% filter(lopnr %in% cancerDiagnosis$lopnr) %>% arrange(lopnr)
# #Finnum fjarlægð á milli unique staka í canmm$lopnr
# uniqueDist <- diff(which(!duplicated(canmm$lopnr)))
# variablesToDuplicate=c("diadat_case","byear","kon","fudeath")
# correctCancerDiagnosis <- canmm %>% cbind(lapply(variablesToDuplicate, function(i) DuplicateVariables(cancerDiagnosis[i])) %>% data.frame()) %>% mutate(lopnr=as.character(lopnr))


#Búum einnig til breytu sem heldur utan um hvada ICD stadal er studst vid fyrir hverja komu sjuklings
#mmcomdat <-  mmcomdat %>% mutate(ICD=cut(INDATUM,breaks=as.Date(c("1958-01-01","1969-01-01","1987-01-01","1997-01-01","2014-01-01")),labels=c("ICD7","ICD8","ICD9","ICD10"),right=FALSE) %>% as.character())
