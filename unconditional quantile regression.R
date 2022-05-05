rm(list=ls(all=TRUE))
library(dplyr)
library(outliers)
library(ggplot2)
library(plm)
library(uqr)
library(panelr)
library(car)
library(tidyverse)
library(stargazer)
setwd("C:/Users/khali/OneDrive/Desktop/attachments")
fr_d <- read.csv("fr2.csv")
fr_d[fr_d < 0] <- NA
fr_d[fr_d$Num..Enterprises==0,] <- NA
fr_d1 <- fr_d[complete.cases(fr_d),]


fr_d1[fr_d1$Fuel.Y>quantile(fr_d1$Fuel.Y,0.99),] <- NA
fr_d1[na.omit(fr_d1$Fert.Y >quantile(!is.na(fr_d1$Fert.Y),0.99)),]   <- NA
fr_d1[na.omit(fr_d1$CP.Y>quantile(fr_d1$CP.Y,0.99, na.rm = T)),] <- NA
fr_d1[na.omit(fr_d1$AEP.Y>quantile(fr_d1$AEP.Y,0.99, na.rm = T)),] <- NA
fr_d1[na.omit(fr_d1$Depr_Int.Y>quantile(fr_d1$Depr_Int.Y,0.99, na.rm = T)),] <- NA
fr_d1=fr_d1[complete.cases(fr_d1),]


qlog=function(x){
  ret=log(x)
  ret[x==0] <- 0
  ret
}

remove(fr_d)

#recreate raw vriables from the intensities
fr_d1$Lab <- fr_d1$L_Q.Y * fr_d1$Total.output/100
fr_d1$Fuel <- fr_d1$Fuel.Y*fr_d1$Total.output
fr_d1$CP <- fr_d1$CP.Y * fr_d1$Total.output
fr_d1$Fert <- fr_d1$Fert.Y*fr_d1$Total.output
fr_d1$AEP <- fr_d1$AEP.Y*fr_d1$Total.output
fr_d1$Depr_Int <- fr_d1$Depr_Int.Y *fr_d1$Total.output



#quasilogs of each Variable 
fr_d1$Q.T.output <- qlog(fr_d1$Total.output) #-ve values here creates NaN
fr_d1$Q.Lab <- qlog(fr_d1$Lab)
fr_d1$Q.Cp <- qlog(fr_d1$CP)
fr_d1$Q.Fuel <- qlog(fr_d1$Fuel)
fr_d1$Q.Fert <- qlog(fr_d1$Fert)
fr_d1$Q.Depr.Int <- qlog(fr_d1$Depr_Int) #-ve values here creates NaN


#Cobb Douglas AUxiliary Regression. 
Cobb_Dog_Prd <- lm(fr_d1$Q.T.output ~ 0+ fr_d1$Q.Lab +
                     fr_d1$Q.Cp+ fr_d1$Q.Fuel + fr_d1$Q.Fert
                   + fr_d1$Q.Fert + fr_d1$Q.Depr.Int)
s_Cobb <-summary(Cobb_Dog_Prd )


myr=coef(Cobb_Dog_Prd)

fr_d1$C_capital= myr[5]* fr_d1$Q.Depr.Int/fr_d1$Total.output
fr_d1$C_lab= myr[1]* fr_d1$Q.Lab/fr_d1$Total.output
fr_d1$C_fuel= myr[3]* fr_d1$Q.Fuel/fr_d1$Total.output
fr_d1$C_CP= myr[2]* fr_d1$Q.Cp/fr_d1$Total.output
fr_d1$C_fert= myr[4]* fr_d1$Q.Fert/fr_d1$Total.output

require(lavaan)

#rescaling Number of enterprises
fr_d1$NE2=fr_d1$Num..Enterprises/10

meas.cmn='
INT=~INT1  + Fuel.Y +  CEI + Depr_Int.Y + NE2 
INT1 =~ CP.Y +Fert.Y
Fuel.Y ~~ CP.Y + Depr_Int.Y + NE2
CEI~~0*CEI
'
summary(fr_d1)

mm=sem(meas.cmn, data=fr_d1)
summary(mm,fit.measures=T)

fr_d1$Intens=predict(mm)[,1]

attach(fr_d1)

pfr_d1 = pdata.frame(fr_d1, index = c("id", "YEAR"))

pooling = plm(Intens~C_capital + C_lab + C_fuel + C_CP  +
                NE2+ C_fert ,  data =pfr_d1, model = "pooling")

sumpool = summary(pooling, diagnose = T)


form = Intens~C_capital + C_lab + C_fuel + C_CP +   NE2+ C_fert 

plmtest(form,data=pfr_d1  , effect= "twoways",type = "bp")
plmtest(form,data=pfr_d1  , effect='individual', type = "bp")
plmtest(form,data=pfr_d1  , effect= "time", type = "bp")

#LM Lagrange test 
phtest(form,data=pfr_d1  , method="chisq")
phtest(form,data=pfr_d1  , method="aux")

within = plm(form,  
             data =pfr_d1, model = "within", effect = "twoway")
summary(within, diagnose = T)


rand = plm(form,  data =pfr_d1, model = "random")

stargazer(within, type = "html", out = "with")
stargazer(pooling, type = "html", out = "pool")
stargazer(rand, type = "html", out = "rand")

summary(rand, diagnose = T)
phtest( betwee, rand)

form1= ~C_capital + C_lab + C_fuel + C_CP +   NE2+ C_fert
range(Intens)


res1=urq(form,data=fr_d1,tau=c(0.1,0.25,0.5,0.75,0.9),id="id", cre = form1)

options(scipen = 999999)

mysumm1 <-urqCI(urq =res1,R=100)
write.csv(mysumm1, "uqr.csv")
