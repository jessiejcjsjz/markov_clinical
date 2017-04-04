
rm(list=ls())

args <- as.numeric(commandArgs(trailingOnly=TRUE))

#Setup
setwd('/zfsauton/project/highmark/data/longoutput')
filename <- dir(pattern='output_out.Medical')[args]
library(data.table)
library(dplyr)

#import dataset
load(filename)
#147.645   0.860 148.939 

vte <- unique(filter(k,VTE=='1')$CI_ID)
imv <- unique(filter(k,IMV=='1')$CI_ID)
control <- unique(k$CI_ID)
control <- control[!control%in%c(vte,imv)]

idlist <- list(vte,imv,control)
save(idlist,file=gsub('output_out','idlist_out',filename))

#Rscript get_id_list.r * &