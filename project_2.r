library(Rmosek)   


Data <- read.csv("/Users/Gopi/gopisaran/train.csv", header=FALSE)
features<-Data[c(51:60,281)]
sampl<-features[sample(1:nrow(features), 7500,replace=FALSE),]
dropcol <- names(sampl) %in% c("V55", "V60")
Data=sampl[!dropcol]
set.seed(11)
sizes<- (nrow(sampl) * 0.75)
train_data <- sample((1:nrow(sampl)), size = sizes)
train <- Data[train_data, ]
test <- Data[-train_data, ]
Train_A=train[,1:ncol(train)-1]
Train_B=train[,ncol(train)]
Train_B=as.matrix(Train_B)
Train_A=as.matrix(Train_A)
Test_A=test[,1:ncol(test)-1]
Test_B=test[,ncol(test)]
Test_B=as.matrix(Test_B)
Test_B=as.matrix(Test_A)
objfun=solve.ols(Train_A, Train_B)
objfun
ycap=Test_A[,1]*-0.06208394 + Test_A[,2]*0.35142553 + Test_A[,3]*(-0.03177670)+ Test_A[,4]*(0.07242577)  + Test_A[,5]*(-1.16527082) +Test_A[,6]*(0.34301559) +Test_A[,7]*(-0.69280633)+  Test_A[,8]* 
error = Test_B-ycap
mse <- function(error)
{
  
  mean(error^2)
  
}
solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]
  xx<-crossprod(X)
  c<--crossprod(X,y)
  xx2<-xx
  xx2[upper.tri(xx)]<-0 
  idx <- which(xx2 != 0, arr.ind=TRUE)
  qo1<-list()
  qo1$sense<-"min" 
  qo1$c<-as.vector(c)
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] )
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE)
  qo1$bc<-rbind(blc=-Inf, buc= Inf)
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p))
  #qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p))
  r<-mosek(qo1, opts = list(verbose = verb))
  return(r)
}

MSE=mse(error)
MSE