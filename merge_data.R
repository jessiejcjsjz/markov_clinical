
rm(list=ls())

#Setup

library(data.table)
library(dplyr)

#import data

setwd("/zfsauton/project/highmark/data/longoutput")
kname <- dir(pattern='subdata_out.Medical')
subdat <- lapply(kname,function(x){
					print(x)
					load(x)
					subdata
				})

#merge data

raw <- as.data.table(do.call(rbind,subdat))
x <- arrange(raw,CI_ID,Date)

#Generate matrix claim*variable

rowid <- paste(x$CI_ID,x$Claim_No)
rowidlist <- unique(rowid)
rowid <- match(rowid,rowidlist)

colid <- x$Var_New
colidlist <- unique(colid)
colid <- match(colid,colidlist)

library(slam)
system.time(modelfile <- slam::simple_triplet_matrix(rowid,colid,x$count))

#save the info in col 1 - 21
x_info <- select(x,-Var_New,-count)
x_info <- unique(x_info)

save(modelfile,x_info,rowidlist,colidlist,file='modelfile_20170402.rda')
