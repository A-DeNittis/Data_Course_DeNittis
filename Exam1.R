## Exam 1

getwd()
setwd("C:/Users/Aly/Desktop/GIT/Data_Course/Exam_1/")
list.files()
library(dplyr)

### Task I ###

dat = read.csv("DNA_Conc_by_Extraction_Date.csv.gz")

jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam1/Katy_Conc_Histogram.jpeg")
plot(dat$DNA_Concentration_Katy, type = "h", xlab = "Sample Number", ylab = "DNA Concentration", main = "DNA Concentration Katy")
dev.off()

jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam1/Ben_Conc_Histogram.jpeg")
plot(dat$DNA_Concentration_Ben, type = "h", xlab = "Sample Number", ylab = "DNA Concentration", main = "DNA Concentration Ben")
dev.off()

### Task II###
str(dat)

plot(as.factor(dat$Year_Collected), dat$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extractions")

plot(as.factor(dat$Year_Collected), dat$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extractions")

### Task III ###

jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam1/DeNittis_Plot1.jpeg")
plot(as.factor(dat$Year_Collected), dat$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extractions")
dev.off()

jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam1/DeNittis_Plot2.jpeg")
plot(as.factor(dat$Year_Collected), dat$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extractions")
dev.off()

### Task IV ###
katy_new_vec = c()
x = 1
for(i in levels(as.factor(dat$Year_Collected))){
  katy_new_vec [x] = mean(dat[as.factor(dat$Year_Collected) == i,"DNA_Concentration_Katy"])
  x = x+1
}


ben_new_vec = c()
x = 1
for(i in levels(as.factor(dat$Year_Collected))){
  ben_new_vec [x] = mean(dat[as.factor(dat$Year_Collected) == i,"DNA_Concentration_Ben"])
  x = x+1
}


### Task V ###
newdat = data.frame(YEARS = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010, 2011, 2012), Ben_Average_Conc = ben_new_vec)
print(newdat$Ben_Average_Conc == max(newdat$Ben_Average_Conc))
maxyear = newdat[8,1]
write.csv(newdat, file = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Exam1/Ben_Average_by_Year.csv", row.names = FALSE)
