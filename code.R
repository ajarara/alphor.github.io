
# libraries used
library(lmtest)
library(MASS)
library(nlme)
library(DAAG) # needed for cross validation

# setup
dat = read.csv("BHD.csv")
mydata = dat # some code in here refers to mydata instead of dat. They're the same thing.

# img/pairNAmes.png
ames = dat[dat$Neighborhood == "NAmes",]
# remove Neighborhood and Street
ames = subset(ames, select = c(-Street,-Neighborhood))
logames = ames
logames$SalePrice = log(ames$SalePrice)
pairs(ames, upper.panel = my_line, main="NAmes")

# img/pairCollgCr.png
coll = dat[dat$Neighborhood == "CollgCr",]
# remove Neighborhood and Street
coll = subset(coll, select = c(-Street,-Neighborhood))
logcoll = coll
logcoll$SalePrice = log(coll$SalePrice)
pairs(coll, upper.panel = my_line, main="CollgCr")

# img/pairTimber.png
timb = dat[dat$Neighborhood == "Timber",]
# remove Neighborhood and Street
timb = subset(timb, select = c(-Street,-Neighborhood))
logtimb = timb
logtimb$SalePrice = log(timb$SalePrice)
pairs(timb, upper.panel = my_line, main="Timber")

# line of best fit function for previous pairs graphs
my_line <- function(x,y,...){
    points(x,y,...)
    abline(a = lm(y ~ x)$coefficients[1] , b = lm(y ~ x)$coefficients[2] , ...)
}

# simple overview
datNoStreet = subset(dat, select = c(-Street,-Neighborhood))
pairs(datNoStreet, upper.panel = my_line, main="Overview")

# Regression overview
logResponse = lm(log(SalePrice)~. - Street, data=dat)
plot(fitted(logResponse), studres(logResponse))

# Weighted model
model_2=lm(log(SalePrice)~.-Street,data=mydata)
u_hat=log(mydata$SalePrice)-fitted(model_2)
u_hat=1/(u_hat**2) 
model_3=gls(log(SalePrice)~.-Street,data=mydata,weights=varFixed(~u_hat))
qqnorm(resid(model_3))
qqline(resid(model_3))

# Intermediary models
model_4=lm(log(SalePrice)~(X1stFlrSF+X2ndFlrSF+
             WoodDeckSF+OpenPorchSF+BsmtSF+Neighborhood+YearBuilt)**2+
             I(X1stFlrSF**2)+I(X2ndFlrSF**2)+
             I(WoodDeckSF**2)+I(BsmtSF**2),data=mydata)
             
#we can see that I(X1stFlrSF^2),X1stFlrSF:X2ndFlrSF is significant,           
summary(model_4)
qqnorm(resid(model_4))
qqline(resid(model_4))


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

# Predictions
selection=rbinom(1460, 1, 0.75)
data_training = mydata[selection == 1,]
data_testing = mydata[selection == 0,]

model_accepted <- lm(log(SalePrice) ~ X1stFlrSF + X2ndFlrSF + WoodDeckSF + OpenPorchSF + Neighborhood + BsmtSF + YearBuilt + I(X1stFlrSF^2) + X1stFlrSF:X2ndFlrSF,data=data_training)

pre_testing = predict(model_accepted, new=data.frame(data_testing))
plot(pre_testing, log(data_testing$SalePrice))

# Cross validation
CVlm(dat,model_final, m=10)
