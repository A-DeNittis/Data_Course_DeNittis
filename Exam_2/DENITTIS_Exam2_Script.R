rm(list = ls())

# Load the libraries that you use here:

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)
library(modelr)


############# Part 1 - Preparing wide data ################## ---------------- (30 points possible)

# read in salaries.csv

setwd("C:/Users/Aly/Desktop/GIT/Data_Course/exam2")
list.files()
dat = read.csv("salaries.csv")

# This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university tier


gdat = gather(dat, key = "Faculty_Rank", value = "Salary", c("AssistProf","AssocProf","FullProf"))

# convert to usable format so we can look at salaries as a dependent variable (10 points)

glimpse(gdat)
gdat$Salary <- as.numeric(gdat$Salary)

# create boxplot of salary by University Tier, colored by Faculty Rank (10 points)
# x-axis = Tier
# y-axis = Salary
# Boxplot fill color = Rank
# Title = "Faculty Salaries - 1995"

p1 = ggplot(gdat, aes(x=Tier, y=Salary, col=Faculty_Rank))+
  geom_boxplot()+
  labs(title = "Faculty Salaries - 1995")


# export this boxplot to a file in your personal repository named "LASTNAME_exam2_plot1.jpeg" (10 points)

ggsave("C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam_2/DeNittis_Exam2_Plot1.jpeg", plot = p1)


################# PART 2 ################### ------------ (70 points possible)

# read in atmosphere.csv
# this data frame has microbial diversity values over time found in atmospheric observation station air filters
# sampling date and two environmental variables [CO2] and [Aerosols] are reported for each measurement
# "Diversity" is the dependent variable

daf = read.csv("atmosphere.csv")
glimpse(daf)

# First, check whether your response variable is normally distributed (5 points)

plot(fitdist(daf$Diversity, distr = "norm")) # Close enough, but can run other distributions too. 


plot(fitdist(log10(daf$Diversity), distr = "norm"))
plot(fitdist(daf$Diversity, distr = "lnorm"))
plot(fitdist(daf$Diversity, distr = "logis")) ## Fits logis distribution best
plot(fitdist(daf$Diversity, distr = "gamma"))


# Next, convert "Year" to a factor...just because (5 points)

daf$Year <- as.factor(daf$Year)
glimpse(daf)

# Create a simple ANOVA model with "Year" as the only explanatory variable (5 points)

mod1 = aov(Diversity ~ Year, data = daf)
summary(mod1)

# Now, create an ANOVA model that incorporates "Year", "Aerosol_Density", and their interaction (5 points)

mod2 = aov(Diversity ~ Year*Aerosol_Density, data = daf)
summary(mod2)

# Compare the two models mean-squared difference method to see which is better at making predictions 
# (20 points)

mod1pred = add_predictions(daf, model = mod1)
mod2pred = add_predictions(daf, model = mod2)

m1 = mean((mod1pred$pred-mod1pred$Diversity)^2)
m2 = mean((mod2pred$pred-mod2pred$Diversity)^2) ## Mean for mod2 is much smaller than mod1, so mod2 is better fit

anova(mod1, mod2)


# Export the summary ANOVA table of the better model to a text file in your repository named:
# "LASTNAME_exam2_table1.txt" (10 points)

sink("C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam_2/DENITTIS_Exam2_Table1.txt")
summary(mod2)
sink(NULL)


# use this model to predict what diversity should be for the following hypothetical conditions:
# note: only include the conditions that are part of your chosen model! (10 points)

# Year = 2007
# Quarter = "Q4"
# Month = August
# Mday = 10
# BarcodeSequence = "CTCTCTATCAGTGAGT"
# Aerosol_Density = 1000,
# CO2_Concentration = 384

new = data.frame(Year = as.factor(2007), Quarter = "Q4", Month = "August", Mday = 10, BarcodeSequence = "CTCTCTATCAGTGAGT",Aerosol_Density = 1000,CO2_Concentration = 384)
conditionpred = predict(mod2, newdata = new)
### 3709.209 

# Now, make a pretty plot to the following specifications:
# x-axis = Day
# y-axis = Aerosol_Density
# point transparency based on values of "Diversity"
# Title: "Decadal Aerosol Density"
# Subtitle: "More aerosols contribute to greater microbial diversity in the atmosphere"

p2 = ggplot(daf, aes(x=Day, y=Aerosol_Density))+
  geom_point(aes(alpha = Diversity))+
  labs(title = "Decadal Aersol Density", subtitle = "More aerosols contribute to greater microbial diversity in the atmosphere")+
  stat_smooth(colour = "darkgreen") +
  theme_bw()
  

# Save this plot in your repository as "LASTNAME_exam2_plot2.jpeg" (10 points)

ggsave("C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam_2/DeNittis_Exam2_Plot2.jpeg", plot = p2)

#### When you are all finished, push the files, including this R script, onto your GitHub repo
#### I will look at your script and look for the three properly named files that you generated


