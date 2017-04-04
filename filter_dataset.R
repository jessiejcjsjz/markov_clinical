
rm(list=ls())

args <- as.numeric(commandArgs(trailingOnly=TRUE))
if(length(args)==0){args<-1}

#Setup

library(data.table)
library(dplyr)

setwd('/zfsauton/project/highmark/data/longoutput')
kname <- dir(pattern='output_out.Medical')[args]
load('control_sample_12345.rda')
load(kname)

#filter dataset
#idlist <- list(vte,imv,control,control_sample,seed=12345)

id_select <- unlist(idlist[c(1,2,4)])
system.time(subdata <- filter(k,CI_ID%in%id_select))
save(subdata,file=gsub('output_out','subdata_out',kname))
