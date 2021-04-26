#Swedish practical ----
#Feb 2021
#Name: William Robson

wants <- c("ggfortify", "here", "nlme", "lattice","ggplot2")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library(nlme)
library(lattice)
library(MASS)
library(here)
library(dplyr)
library(ggplot2)



# load data and check the top 6 rows. 

rawd<-read.csv("sweden_simple.csv")
head(rawd)
nrow(rawd)
ncol(rawd)

#Exploring the data with ggplot and xplot
ggplot(rawd, aes(time,red, colour=as.factor(name)))+
geom_point(size=0.6, alpha=1)+ 
facet_wrap(~name)+
xlab("Time (years)")+
ylab("Number of foxes")+ 
labs(color='Swedish County Names')+
geom_smooth(method =glm)+
theme_dark() 

xyplot(red~time|name, rawd, typ=c("a","b"), xlab="Time (years)", ylab="Fox numbers")

#Plotting the number of wolves using xyplot

xyplot(log(wolf+1)~time|name,rawd, typ=c("a","b"))

#Are there any trends with time and county? Use a GLS (generalised least squares)
fox_trends.gls<-gls(log(red)~time+name, rawd)

summary(fox_trends.gls)

#There is a few significant counties

#Looking at the fitted model by county 
#This is just looking at how well the points fit the line set into graph
xyplot(exp(fitted(fox_trends.gls))+red~time|name, rawd, xlab="Time in years", ylab=)

#Lets also look at the residuals
#As can see the tails are long with some at -5.8 (or thereabouts)
#Still a reasonable model with an almost normal distribution
truehist(residuals(fox_trends.gls))

#The problem with the previous model is that we have not allowed for autocorrelation 
#This basically is the dependence of the response on previous years.
plot(ACF(fox_trends.gls))

#We can see that there is considerable autocorrelation. Meaning that the significance 
#will be biased upwards. We adjust using the correlation statement.
#We have to define what sort of correlation exists. we will use the simple autoregressive which assumes that part of the observed pattern is driven by the last measurement (here the previous bag). 
#We will also assume that this varies by county.

fox_trends_auto.gls<-gls(log(red)~time+name,correlation=corAR1(form=~1|name), rawd)
#Note phi gives the estimate of the serial dependence between measures
summary(fox_trends_auto.gls)

#We need to compare these two models to see which one is better
anova(fox_trends.gls,fox_trends_auto.gls)
AIC(fox_trends_auto.gls, fox_trends.gls)

#The autocorrelated version is a much better model in comparison to the normal version

#Mixed effects modeling ----
#This a differnt form of model that will allow us to see the differnces. 
#The other models assume that the phenomena is there for all counties, when we know it is not

red_fox.lme<-lme(log(red)~time, random=~1|name, rawd)
summary(red_fox_auto.lme)

#0.898/(0.898+0.600) = 0.59 i.e. 59% of the residuals/errors can be accounted for! 

xyplot(exp(fitted(red_fox.lme))+red~time|name, rawd)

#A really good model showing how the observed lies on the predicted

#Investigating the residuals
qqnorm(red_fox.lme, ~resid(.,type="p")|name, abline=c(0,1))
#All very good and lying on the line
#The issue is we have forgot to autocorrelate

red_fox_auto.lme<-lme(log(red)~time, random=~1|name,correlation=corAR1(form=~1|name), rawd)

truehist(residuals(red_fox_auto.lme), xlab="Residuals for red foxes", ylab="Values")

xyplot(exp(fitted((red_fox_auto.lme)))+red~time|name, rawd, xlab="Time (years)", ylab="Number of Red Fox shot")

anova(red_fox.lme,red_fox_auto.lme)
AIC(red_fox.lme, red_fox_auto.lme)
summary(red_fox_auto.lme)
#The auto model is so much better when compared to the original one 

#Now to do it for Wolves and Lynx ----
#Making a new dataframe for only Wolves and Lynx
colnames(rawd)

rawd$top_predators_log <- log(rawd$wolf+1) + log(rawd$lynx+1)

colnames(rawd)

rawd
#Now we can do some new analysis
xyplot(top_predators_log~time|name, rawd, typ=c("a","b"),xlab="Time in log scale (years)", ylab="Predator levels in log scale")

ggplot(rawd, aes(time,top_predators_log, colour=as.factor(name)))+geom_point(size=0.6, alpha=1) + facet_wrap(~name) +xlab("Lynx/Wolf numbers in log scale") + ylab("Time (years) in log scale")+ labs(color='Swedish County Names') + geom_smooth(method =glm) +theme_bw() 
#Analysing the relationships
predator_trends.gls<-gls(top_predators_log~time+name, rawd)
summary(predator_trends.gls)
#A large amount of predictors are significant
#Lets look at this in a model
xyplot(exp(fitted((predator_trends.gls))-1)+top_predators~time|name, rawd)

#Checking the residuals of this model
truehist(residuals(predator_trends.gls))

#Quite a normal distribution, with a long tail on the positive end. (centred around 0)
#It also is overpredicting quite a few values.

#We need to check for autocorrelation 
plot(ACF(predator_trends.gls))
#There is lots of autocorrelation in this model. 

predator_trends_auto.gls<-gls(top_predators_log~time+name,correlation=corAR1(form=~1|name), rawd)
summary(predator_trends_auto.gls)

#The phi value is 0.78

#Mixed effect modeling predators ----
predators.lme<-lme(top_predators_log~time, random=~1|name, rawd) 
summary(predator_auto.lme)
#0.19/0.48+0.19 = 0.59 We can account for 59% of the errors
#Intercept/residual +intercept
0.0003/(0.53+0.0003)
xyplot(exp(fitted((predators.lme))-1)+top_predators_log~time|name, rawd)

truehist(residuals(predator_auto.lme), xlab="Residuals for predators", ylab="Values")

qqnorm(predators.lme, ~resid(.,type="p")|name, abline=c(0,1))

predator_auto.lme<-lme(top_predators_log~time, random=~1|name,correlation=corAR1(form=~1|name), rawd) 

summary(predator_auto.lme)

xyplot(exp(fitted((predator_auto.lme))-1)+top_predators_log~time|name, rawd, xlab="Time (years)", ylab="Number of predators shot in log scale")

anova(predator_auto.lme,predators.lme)
AIC(predator_auto.lme,predators.lme)

#Auto is much better with a lower score

#Mixed effect modeling seed levels ----
seed.lme<-lme(seed~time, random=~1|name, rawd) 
summary(seed.auto.lme)
12.4/(13336.4+12.4) = 0.0009

xyplot(fitted(seed.auto.lme)+seed~time|name, rawd, xlab="Time (years)", ylab="Seed baskets sown")

qqnorm(seed.auto.lme, ~resid(.,type="p")|name, abline=c(0,1))

seed.auto.lme<-lme(seed~time, random=~1|name,correlation=corAR1(form=~1|name), rawd)

truehist(residuals(seed.auto.lme), xlab="Residuals for seed", ylab="Values")

anova(seed.auto.lme,seed.lme)
AIC(seed.auto.lme,seed.lme)

#Auto is much better with a lower score