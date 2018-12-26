library(caTools)
library(leaps)
library(car)
library(alr3)
library(class)
attach(ncaaf)
library(MASS)
head(ncaaf)
names(ncaaf)
model1=lm(line~fro1, data=ncaaf)
model2=lm(line~fro1+fpo1+fto1+fso1+frd1+fped1+ftd1+fsd1+fsched+uro1+upo1+uto1+
            uso1+urd1+uped1+utd1+usd1+usced+power5+AnyRivalry+
           frank+urank+fnr+unr+Home_Rating+Away_Rating+etp+finj+uinj, data=ncaaf)
summary(model2)
vif(model2)


library(car)
n=length(model1$residuals)
backAIC <- step(model2,line="backward", data=ncaaf)
backBIC <- step(model2,line="backward", data=ncaaf, k=log(n))
ncaaf.aic.back=lm(line~urd1+fsched+fsd1+etp+frank+fso1+uto1+urank+
                    usd1+frd1+uso1+unr+usced+Home_Rating+fnr+Away_Rating, data=ncaaf)
ncaaf.bic.back=lm(line~uto1+frank+uso1+urank+usced+frd1+unr+
                    Away_Rating+fnr+Home_Rating+fso1+usd1,data=ncaaf)
summary(ncaaf.bic.back)
summary(ncaaf.aic.back)
vif(ncaaf.aic.back)
ncaaf.vegas.wrong=lm(line~fped1+utd1+power5+AnyRivalry+finj+uinj,data=ncaaf)
summary(ncaaf.vegas.wrong)
ncaaf.vegas.wrong2=lm(line~fped1+utd1+power5+AnyRivalry+finj+uinj,data=ncaaf)
summary(ncaaf.vegas.wrong2)
vif(ncaaf.vegas.wrong2)
ncaaf.vegas.wrong3=lm(line~power5+AnyRivalry+finj,data=ncaaf)
summary(ncaaf.vegas.wrong3)
vif(ncaaf.vegas.wrong3)
anova(ncaaf.vegas.wrong3,model2)

detach(ncaaf)
