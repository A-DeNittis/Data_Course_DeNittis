## Cleaning data

getwd()
setwd("C:/Users/Aly/Desktop/GIT/Data_Course/data")
list.files()
dat = read.csv("BioLog_Plate_Data.csv")

library("tidyr")
## Looking at data

levels(dat$Sample.ID)
unique(dat$Rep)
unique(dat$Dilution)
levels(dat$Substrate)


absorb = c(dat$Hr_24, dat$Hr_48, dat$Hr_144)

newdat = (dat[, -c(6,7,8)])

newdf = data.frame(x = c(1,1,2,2,3,3,4,4,5,5,6,6), y = c(1:12), y2 = (rep(1:4,3)))
gath = gather(newdf, key = "xy", value = "zz", c("y","y2"))

###### GATHER ###############
# gather(dataset, key = title of new combined column, value = name of new value column, names of columns to be combined)
#############################
###Learn GATHER

gdat = gather(dat, key = "Time", value = "Abs", c("Hr_24","Hr_48","Hr_144"))


ggplot(gdat, aes(x=Time, y=Abs, col=Sample.ID)) +
geom_point() +
stat_smooth()


h24 = which(gdat$Time == "Hr_24")
h48 = which(gdat$Time == "Hr_48")
h144 = which(gdat$Time == "Hr_144")

gdat$Time = as.numeric(mapvalues(gdat$Time, from = c("Hr_24","Hr_48", "Hr_144"), to = c(24, 48, 144)))

for (i in levels(gdat$Substrate)){
  sub = subset(gdat, Substrate == i)  
    print(ggplot(data = sub, aes_string(x="Time", y="Abs", col = "Sample.ID"))+
    geom_point()+
    stat_smooth()+
    ggtitle(i))
}

warnings()    

dev.off()

