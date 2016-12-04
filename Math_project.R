mydata=read.csv("C:/Users/zhezhi hou/Downloads/BHD.csv")
#check whether is a variance in Pave:
library(lmtest)
library(MASS)
library(nlme)
attach(mydata)
type_street=unique(Street)#there is a variance in Pave
Street=as.factor(Street)
Neighborhood=as.factor(Neighborhood)
YearBuilt=factor(YearBuilt)
is.factor(YearBuilt)
is.numeric(YearBuilt)
model_1=lm(SalePrice~.,data=mydata)
summary(model_1)
plot(fitted(model_1),resid(model_1))
#find heteroskedicity
#use log transformation
model_2=lm(log(SalePrice)~.,data=mydata)
summary(model_2)
plot(fitted(model_2),studres(model_2))
abline(h=0,lty=2)
plot((1:nrow(mydata)),studres(model_2))
#it seem log transform relieve the heteroskedicity problem
#take log in response interpretation:
#a 1-unit change in X corresponds to (approximately) 
#an expected increase in Y of 6%.(beta=.06)
#check normality for error term:
qqnorm(studres(model_2))
qqline(studres(model_2))
#we need normality assumaption to make our t test to be 
#valid, but from previous graph, our error is not from
#normal
#we need to use GLS to make the t test to be valid
#The GLS estimator is unbiased, consistent, efficient
#check the independence of error term
#durbin Watson Test
dwtest(model_2)
#no correlation
u_hat=log(SalePrice)-fitted(model_2)
u_hat=1/(u_hat**2) 
model_3=gls(log(SalePrice)~.,data=mydata,weights=varFixed(~u_hat))
qqnorm(resid(model_3))
qqline(resid(model_3))
#we can see that normality assumption holds now.t test is valid
#we can determine now which variable is not significant
summary(model_3)
#this variables is not significant:
#OpenPorchSF StreetPave NeighborhoodBlueste 
#NeighborhoodClearCr NeighborhoodCollgCr
#NeighborhoodCrawfor NeighborhoodGilbert
#NeighborhoodNoRidge NeighborhoodNPkVill
#NeighborhoodSomerst NeighborhoodTimber
#NeighborhoodVeenker
#from the above results, we can see that 
#variables significantly affect housing price in Boston
#is NeighborhoodIDOTRR 

#find the best model to fit:
#try polynomials and cross term
# X1stFlrSF+X2ndFlrSF+WoodDeckSF+OpenPorchSF+BsmtSF
#Street+Neighborhood+YearBuilt
model_4=lm(log(SalePrice)~(X1stFlrSF+X2ndFlrSF+
             WoodDeckSF+OpenPorchSF+BsmtSF+Street+Neighborhood+YearBuilt)**2+
             I(X1stFlrSF**2)+I(X2ndFlrSF**2)+
             I(WoodDeckSF**2)+I(BsmtSF**2),data=mydata)
             
#we can see that I(X1stFlrSF^2),X1stFlrSF:X2ndFlrSF is significant,           
summary(model_4)
qqnorm(resid(model_4))
qqline(resid(model_4))
#although we include all the polynomials and cross term. the error is still not normal
#whick means that we can not use anova analysis, we will try different subset to see whether
#their error is normal or not. we use the X2ndFlrSF to seperate the data into two groups
data_1=mydata[mydata$X2ndFlrSF > 0,]
#=data_1[data_1$WoodDeckSF > 0,]
model_51=lm(log(SalePrice)~.+I(X1stFlrSF**2)+X1stFlrSF:X2ndFlrSF-Street,
            data=data_1)
summary(model_51)
model_52=stepAIC(model_51,direction="both")
#we can get the following model which is best fit
#log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF + 
 # Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2)
data_2=mydata[mydata$X2ndFlrSF == 0,]
model_61=lm(log(SalePrice)~.+I(X1stFlrSF**2)-Street,
            data=data_2)
summary(model_61)
model_62=stepAIC(model_61,direction="both")
#we can get the following model which is best fit
#log(SalePrice) ~ X1stFlrSF + WoodDeckSF + OpenPorchSF + Neighborhood + 
 # BsmtSF + YearBuilt + I(X1stFlrSF^2) 
#combine the two results, we get:
#log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF + 
# Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2)
model_final=lm(log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF +
                 Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2),data=mydata)
summary(model_final)

#examine the results:
data_random=rbinom(1460,1,0.5)
data_exm=cbind(mydata,data_random)
data_exm1=data_exm[data_exm$data_random==0,]
data_true=data_exm[data_exm$data_random==1,]
model_exm<-lm(log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF +
                 Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2),data=data_exm1)
pre_true=predict(model_exm,new=data.frame(data_true))
plot(pre_true,log(data_true$SalePrice))
#we can see that it predict very well.

#the problem which I interested:
#if we delete the outliers, will it infect our est_coeff?
#find the outliers 
mydata_new=data.frame(mydata,log(mydata$SalePrice))
mydata_new$SalePrice=NULL
boxplot(mydata_new)
n=length(X1stFlrSF)
X1stFlrSF_1=sort(X1stFlrSF)
X1stFlrSF_2=X1stFlrSF_1[1401:1460]# delete the 60 largest value
index_X1st=matrix(0,60,1)
for (i in 1:60)
{
index_X1st[i]<-match(X1stFlrSF_2[i],X1stFlrSF)
}
WoodDeckSF_1=sort(WoodDeckSF)
WoodDeckSF_2=WoodDeckSF_1[1401:1460]# delete the 60 largest value
index_Wood=matrix(0,60,1)
for (i in 1:60)
{
  index_Wood[i]<-match(WoodDeckSF_2[i],WoodDeckSF)
}
OpenPorchSF_1=sort(OpenPorchSF)
OpenPorchSF_2=OpenPorchSF_1[1401:1460]# delete the 60 largest value
index_Open=matrix(0,60,1)
for (i in 1:60)
{
  index_Open[i]<-match(OpenPorchSF_2[i],OpenPorchSF)
}
BsmtSF_1=sort(BsmtSF)
BsmtSF_2=BsmtSF_1[1451:1460]# delete the 10 largest value
index_Bsmt=matrix(0,10,1)
for (i in 1:10)
{
  index_Bsmt[i]<-match(BsmtSF_2[i],BsmtSF)
}
indx=c(index_X1st,index_Open,index_Wood,index_Bsmt)
index=unique(indx)
mydata_new2=mydata[-index, ]
model_outlier=lm(log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF +
                 Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2),data=mydata_new2)
summary(model_outlier)
qqnorm(resid(model_outlier))
qqline(resid(model_outlier))
