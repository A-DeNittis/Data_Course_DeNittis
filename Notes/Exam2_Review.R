x = rnorm(10)
y = rnorm(10)
z = rnorm(10)

obs = 1:10

df = data.frame(obs = obs, x=x, y=y, z=z)

df.l = gather(df, key = "Stock", value = "PriceChange", c("x","y","z"))

spread(df.l, Stock, PriceChange)


aova1 = aov(PriceChange~Stock*obs, data = df.l)
summary(aova1)

TukeyHSD(aova1)
summary(aova1)
str(aova1)

# dplyr:: double colon lists actions
# PIPE %>% takes output of anything on left to feed to right

df %>% mutate(total = x+y+z, minimum = min(c(x, y, z)), newx = x)
#mutate adds new or replaces existing columns

apply(df[,2:4],1,min)

mutate()
select(df, c(x,y))
df %>% select(c(x,y))

filter

# select picks columns
# filter picks rows

df$group = c(rep("A", 5), rep("B",5))

group_by(df, group) %>% summarise(meanx = mean(x))         


df2 = read.csv("C:/Users/Aly/Desktop/GIT/Data_Course/data/iris.csv")
mod1 = aov(Petal.Length ~ Petal.Width*Species, data = df2)
mod2 = aov(Petal.Length ~ Petal.Width+Species, data = df2)

summary(mod1)
summary(mod2)

anova(mod1,mod2)

df2 = add_predictions(df2,mod1, var = "mod1")
df2 = add_predictions(df2, mod2, var = "mod2")
 
p1 = ggplot(df2)+
   geom_point(aes(x=Petal.Width, y=Petal.Length, color = Species))+
   geom_smooth(aes(x=Petal.Width, y=mod1, col = Species),method = "lm") +
  geom_smooth(aes(x=Petal.Width, y=mod2, col = Species),method = "lm", linetype = 5) 

sqrt(mean((df2$mod1 - df2$Petal.Length)^2)) 
sqrt(mean((df2$mod2 - df2$Petal.Length)^2))
##Model 1 mean was lower, so points on graph were closer to linear model


## Make long dataset
## Build some models, see which better
## Make some plots
## ANOVA table