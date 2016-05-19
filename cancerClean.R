library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plyr)
library(xlsx)
Sys.setlocale(category="LC_ALL",locale="is_IS")
setwd("../data")
load("canmm.RData")
classChanges <- function(i){
    i <- as.character(i)
    i[is.na(i)] <- ""
    i
}
#Sameining ICD breyta yfir i diag
canmm <- canmm %>% ungroup() %>% mutate_each(funs(classChanges),ICD7,ICD9) %>% gather(ICD,diag,starts_with("ICD"))
canmm <- canmm  %>% select(lopnr,diadatn,diag) %>% rename(c(diadatn="INDATUM"))