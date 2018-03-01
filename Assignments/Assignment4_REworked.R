library(ggplot2)
library(broom)
library(fitdistrplus)

df = read.csv("C:/Users/Aly/Desktop/GIT/Data_Course/data/mushroom_growth.csv")
glimpse(df)


ggplot(df, aes(x=GrowthRate))+
  geom_histogram()

plot(fitdist(df$GrowthRate, distr = "norm"))

ggplot(df, aes(x=Nitrogen, y=GrowthRate, col=Species))+
  geom_point() +
  stat_smooth()+
  facet_grid(facets = ~Humidity)

ggplot(df, aes(x=Light, y=GrowthRate, col=Species))+
  geom_point() +
  stat_smooth()+
  facet_grid(facets = ~Humidity)

##Difference between species only seen in high humidity

mod1 = aov(GrowthRate ~ Light*Species*Humidity*Nitrogen, data = df) ##Sequential series can produce errors, subject to bias
summary(mod1) ## Light is significant, 

pc = subset(df, Species == "P.cornucopiae")

mod2 = aov(GrowthRate ~ Light*Humidity*Nitrogen, data = pc)
summary(mod2)

ggplot(pc, aes(x=Nitrogen, y=GrowthRate, col=Humidity))+
  geom_point() +
  stat_smooth(method = "lm") ##linear model similiar to what ANOVA is looking at

mod3 = aov(GrowthRate ~ Light*Humidity, data = pc)
summary(mod3)
 mod4 = aov(GrowthRate ~ Light*Humidity*Temperature, data = pc)

library(modelr)
mod3pred = add_predictions(pc, model = mod3)
mod4pred = add_predictions(pc, model = mod4)

mean((mod3pred$pred-mod3pred$GrowthRate)^2)
mean((mod4pred$pred-mod4pred$GrowthRate)^2)


anova(mod3, mod4)

new = data.frame(Light = 30, Humidity="High")
predict(mod3, newdata = new)



ggplot(pc, aes(x=Temperature, y=GrowthRate, col = Humidity))+
  geom_point()+
  stat_smooth(method = "lm")

####################### OUTPUT

## Nice graph

ggplot(pc, aes(x=Light, y=GrowthRate, col = Humidity)+
  geom_boxplot()

ggplot(pc, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature)))+
  geom_jitter(width = .7 )+
  geom_point()+
  labs(y="Growth Rate", title = "Model 4")+
  scale_linetype_discrete(name = "Temperature (Degrees Celcius)")+
  stat_smooth(method = "lm", se = FALSE)+
  theme_bw()
  


sink("C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/ANOVA_table.txt")
summary(mod4)
sink(NULL)


tidy(mod4)
summary(mod4)


##### Notes

### load gapminder data to rework for Assignment 4
