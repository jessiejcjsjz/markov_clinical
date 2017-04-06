
rm(list=ls())
setwd('/zfsauton/project/highmark/data/longoutput/featureselectresult')
load('vte_select_cum0405.rda')

model.lda <- MASS::lda(VTE_t1~.,data=mf.wh)
vali.lda <- table(predict(model.lda)$class,mf.wh$VTE_t1)
sum(diag(vali.lda))/sum(vali.lda)

library(xgboost)
model.xgb <- xgboost(
	data = data.matrix(mf.wh[,-104]), 
	label = mf.wh[,104], 
	max.depth = 5, 
	eta = 1,
	nround = 2, 
	objective = "binary:logistic")
pred.xgb <- predict(model.xgb,data.matrix(mf.wh[,-104]))
model.xgb2 <- MASS::lda(mf.wh$VTE_t1~pred.xgb)
pred.xgb2 <- predict(model.xgb2)
vali.xgb <- table(pred.xgb2$class,mf.wh$VTE_t1)
sum(diag(vali.xgb))/sum(vali.xgb)

library(mxnet)
model.mlp <- mx.mlp(
	data.matrix(mf.wh[,-104]), 
	mf.wh[,104], 
	hidden_node=10, 
	out_node=2,      
	out_activation="softmax", 
	num.round=20, 
	array.batch.size=15, 
	learning.rate=0.07, 
	momentum=0.9, 
	eval.metric=mx.metric.accuracy)
