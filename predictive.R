
rm(list=ls())
setwd('/zfsauton/project/highmark/data/longoutput/featureselectresult')
load('vte_select_cum0405.rda')

model.lda <- MASS::lda(VTE_t1~.,data=mf.wh)
vali.lda <- table(predict(model.lda)$class,mf.wh$VTE_t1)
sum(diag(vali.lda))/sum(vali.lda)

library(xgboost)
model.xgb <- xgboost(
	data=as.matrix(mf.wh[,-104]),
	label=mf.wh[,104],
	eta = 0.1,
	max_depth = 15, 
	nround=25, 
	subsample = 0.5,
	colsample_bytree = 0.5,
	seed = 1,
	eval_metric = "merror",
	objective = "multi:softprob",
	num_class = 12,
	nthread = 3)
vali.xgb <- table(predict(model.xgb,as.matrix(mf.wh[,-104])),mf.wh$VTE_t1)
sum(diag(vali.xgb))/sum(vali.xgb)

