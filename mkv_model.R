
#Modeling with Markov Chain

rm(list=ls())

#Setup

library(data.table)
library(dplyr)
library(slam)

#import data

setwd("/zfsauton/project/highmark/data/longoutput")
load('modelfile_20170402.rda')
claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))
load('control_sample_12345.rda')

#subdata for Y1

i <- 1
	#get subdata for those samples with Yi
datai <- modelfile[which(claim[,1]%in%as.numeric(idlist[[i]])),]
datai <- as.data.table(as.matrix(datai))
colnames(datai) <- colidlist

	#Claimi and ID information for Yi, coding T
claimi <- claim[claim[,1]%in%as.numeric(idlist[[i]]),]
claimi <- cbind(claimi,ordert1=1)
for(k in 2:nrow(claimi)){
	if(claimi[k,1]==claimi[k-1,1]){
		claimi[k,3] <- claimi[k-1,3]+1
	}
}
claimi <- cbind(claimi,ordert0=claimi[,3]-1)

t0 <- paste(claimi[,1],claimi[,4])
t1 <- paste(claimi[,1],claimi[,3])

	#get xinfo subdata
xcode <- paste(x_info[,1],x_info[,2])
claimi <- cbind(claimi,getxinfo=paste(claimi[,1],claimi[,2])%in%xcode)

xcode <- x_info[match(paste(claimi[,1],claimi[,2]),xcode),]
data1 <- data.table(xcode,datai)
data0 <- data1[match(t0,t1)]
colnames(data0) <- paste0(colnames(data0),'_t0')

rowsel <- (!is.na(match(t0,t1)))&(!is.na(claimi[,5])) #t0 could be matched and xinfo could be found
data1 <- data1[which(rowsel)]
data0 <- data0[which(rowsel)]
save(data0,data1,file='testfile_yi.rda')

#modeling file

rm(list=ls())
library(data.table)
library(plyr)

setwd("/zfsauton/project/highmark/data/longoutput")
load('testfile_yi.rda')

data0$VTE_t0 <- ifelse(is.na(as.numeric(data0$VTE_t0)),0,1)
data0$VTE_t1 <- ifelse(is.na(as.numeric(data1$VTE)),0,1)
mf <- apply(data0,2,function(x){ifelse(is.na(as.numeric(x)),0,as.numeric(x))})#convert all the cell into numerical

#rm(list=ls())
#library(data.table)
#library(plyr)
#setwd("/zfsauton/project/highmark/data/longoutput")
#load('mf_vte.rda')

mf2 <- as.data.frame(mf)[,as.numeric(which(apply(mf,2,var)>0))] #kick out all the variable with no variation

i <- 0
feature_selection <- apply(mf2,2,function(x){
	print(i<<-i+1)
	try(t.test(x~mf2$VTE_t1))
}) #Test the correlation between all the t0 variable with vte_t1

fs_out <- c(sapply(feature_selection[-length(feature_selection)],function(x) x$p.value),0)
mf2_featured <- mf2[,fs_out<=(0.05/length(feature_selection)),drop=F]
#test <- MASS::lda(VTE_t1~.,data=mf2_featured)
#table(predict(test)$class,mf2_featured$VTE_t1)

feature_selection2 <- 
	sapply(1:100,function(i){
		print(i)
		set.seed(i); train <- sample(1:nrow(mf2),nrow(mf2)*0.5)
		train_data <- mf2_featured[train,]
		apply(train_data,2,function(x){t.test(x~train_data$VTE_t1)$p.value})
	})

	
