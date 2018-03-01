##Assignment 4 
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)
library(modelr)

setwd("C:/Users/Aly/Desktop/GIT/Data_Course/data/")
list.files()

#######

dat = read.csv("mushroom_growth.csv")
glimpse(dat)
str(dat)


##Subset species
dat1 = subset(dat, dat$Species == "P.ostreotus")
dat2 = subset(dat, dat$Species == "P.cornucopiae")

# Distributions/Exploratory
plot(fitdist(dat$GrowthRate, distr = "norm"))
plot(fitdist(dat$GrowthRate, distr = "lnorm"))
plot(fitdist(dat$GrowthRate, distr = "logis"))
plot(fitdist(dat$GrowthRate, distr = "gamma"))



# Log model fit to norm dist. better
plot(fitdist(log10(dat$GrowthRate), distr = "norm"))
dat$Log10_GrowthRate <- log10(dat$GrowthRate) ##Add to dataframe

#Graphs for each variable against growth rate
for(i in names(dat)){
  plot(dat[,"GrowthRate"] ~ dat[,i], xlab = i, ylab = "Growth Rate", main = i)
}

#Model

model = lm(Log10_GrowthRate ~ Nitrogen + Light + Humidity + Temperature, data = dat)
p = predict(model, dat)
plot(p - dat$Log10_GrowthRate)

modl= aov(Log10_GrowthRate ~ Nitrogen*Light*Humidity*Temperature, data = dat)
summary(modl)
plot(modl)
tidy(modl)

## Light and humidity seem to be the factors that influence growth rate the most ##

boxplot(dat$Log10_GrowthRate~dat$Light)
boxplot(dat$Log10_GrowthRate~dat$Nitrogen)

dat = add_predictions(dat, model = modl) #Generate predictions
plot(dat$Log10_GrowthRate, dat$pred) #Plot predictions
abline(lm(dat$pred ~dat$Log10_GrowthRate)) #Fit linear model

m = mean(sum((dat$Log10_GrowthRate - dat$pred)^2))


TukeyHSD(modl) #Tukey test, sets of confidence intervals
warnings()




# modelFunction(GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature, data = dat)


dat$Cross = rnorm(length(dat$Species))
dat.sample = sample(dat$Cross, 108)
dat.train = dat[which(dat$Cross %in% dat.sample),]
dat.cross = dat[which(dat$Crorss %in% dat.sample),]

modl3 = aov(Log10_GrowthRate~Light*Nitrogen*Temperature*Humidity, data = dat)
plot(modl3)
  
  
summary(aov(modl3))

(anova(modl, modl3))

cross.predictions = add_predictions(model=modl3, data=dat.cross)
mean((cross.predictions$pred - cross.predictions$Log10_GrowthRate)^2)


####
plot(dat$GrowthRate~dat$Light*dat$Nitrogen)

ggplot(dat, mapping = aes(y=Log10_GrowthRate, xlab=""))+
  geom_col(aes(x=Light, y=Log10_GrowthRate, fill="Light (hours/day)"))+
  geom_col(aes(x=Nitrogen, y=Log10_GrowthRate, fill = "Nitrogen (mg/g soil)"))+
  xlab("")+
  ylab("Growth Rate")


