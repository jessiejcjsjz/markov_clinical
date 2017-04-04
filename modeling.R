
rm(list=ls())

#Setup

library(data.table)
library(dplyr)
library(slam)

#import data

setwd("/zfsauton/project/highmark/data/longoutput")
load('modelfile_20170402.rda')

claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))
id <- unique(claim[,1])
i <- 0
modelfile_byid <- lapply(id,
	function(idi){
		i <<- i+1
		print(i/length(id))
			x <- data.matrix(modelfile[which(claim[,1]==idi),])
			rbind(apply(x,2,cumsum))
			})

processedfile <- do.call(rbind,modelfile_byid)
save(processedfile,colidlist,rowidlist,x_info,file='processed.rda')

#Rscript modeling.R > process_modelfile.log &
