# Assignment Week 3




# topics:   type conversions, factors, plot(), making a data frame from "scratch",
#           reordering, 


# vector operations!

vector1 = c(1,2,3,4,5,6,7,8,9,10)
vector2 = c(5,6,7,8,4,3,2,1,3,10)

vector1*vector2
vector1+1

vector3 = c("radish","coffee","grub")
vector4 = c("stuff","junk","things")

c(vector3, vector4)

?rep #rep replicates the values in x. It is a generic function, and the (internal) default method is described here.
rep(vector3,3)

#############################################################################


setwd("C:/Users/Aly/Desktop/GIT/Data_Course/data/")
list.files()

dat = read.csv("thatch_ant.csv")
names(dat)

#why are these plots different???
plot(x=dat$Headwidth..mm., y=dat$Mass)
plot(x=dat$Size.class, y=dat$Mass)


#check the classes of these vectors
class(dat$Headwidth..mm.)
class(dat$Size.class)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat)
?str #Display structure of object



# Two of them are "Factor" ....why is the column "Headwidth" a factor? It looks numeric!
levels(dat$Headwidth)
?levels
# we can try to coerce one format into another with a family of functions
# as.factor, as.matrix, as.data.frame, as.numeric, as.character, as.POSIXct, etc....

#make a numeric vector to play with:
nums = c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console
nums_factor = as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it


#check it out
plot(nums) 
plot(nums_factor)
plot(nums_factor,nums)
# take note of how numeric vectors and factors behave differently in plot()

# Let's modify and save these plots. Why not!?
?plot()
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label", col = c(1,2))


?jpeg() #run next 3 lines together
jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_Denittis/TestPlot.jpeg")
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label", col = c(1,2))
dev.off()
?dev.off


# back to our ant data...
dat$Headwidth
levels(dat$Headwidth) # levels gives all the "options" of a factor you feed it


# I notice a couple weird ones in there: "" and "41mm"
# The "" means a missing value, basically. The "41mm" sure looks like a data entry error.
                                            # It should probably be "41.000"

# FIND WHICH ONES HAVE "41mm"
which(dat$Headwidth == "41mm")

# CONVERT THOSE TO "41.000"
a = which(dat$Headwidth == "41mm")
dat$Headwidth[a] = "41.000"

dat$Headwidth = as.numeric(dat$Headwidth)

class(dat$Headwidth)
plot(dat$Headwidth, dat$Mass)


# DO THE SAME FOR "", BUT CONVERT THOSE TO "NA"

which(dat$Headwidth == NA)
summary(dat$Headwidth)
plot(dat$Headwidth)
bad = which(dat$Headwidth == 1)
dat$Headwidth[bad] = NA

dat2 = na.omit(dat)


# NOW, CONVERT THAT PESKY "Headwidth" COLUMN INTO A NUMERIC VECTOR WITHIN "dat"
class(dat2$Headwidth)
datvector = c(dat2$Headwidth)

# LET'S LEARN HOW TO MAKE A DATA FRAME FROM SCRATCH... WE JUST FEED IT VECTORS WITH NAMES!

# make some vectors *of equal length* (or you can pull these from existing vectors)
col1 = c("hat", "tie", "shoes", "bandana")
col2 = c(1,2,3,4)
col3 = factor(c(1,2,3,4)) # see how we can designate something as a factor             
col4 = c(1:4)

# here's the data frame command:
data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # colname = vector, colname = vector....
df1 = data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # assign to df1
df1 # look at it...note column names are what we gave it.



# Make a data frame from the first 20 rows of the ant data that only has "Colony" and "Mass"
# save it into an object called "dat3"

row.names(dat2)
?head # n is number of elements for a vecter, n=20 will return first 20 lines, n=5 5 lines, etc.
colColony = head(c(dat2$Headwidth), n=20)
colMass = head(c(dat2$Mass), n=20)
ColMassDF = data.frame(Colony = colColony, Mass = colMass)
write.csv(ColMassDF, file = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/ColMassDF_Asgnt3Practice.csv", row.names = FALSE)

###### WRITING OUT FILES FROM R #######
?write.csv()


# Write your new object "dat3" to a file named "LASTNAME_first_file.csv" in your PERSONAL git repository
write.csv(dat2, file = "C:/Users/Aly/Desktop/Data_Course_DeNittis/DeNittis_first_file.csv", row.names = FALSE)



### for loops in R ###

#simplest example:
for(i in 1:10){
  print(i)
}

#another easy one

for(i in levels(dat2$Size.class)){
  print(i)
}

# can calculate something for each value of i ...can use to subset to groups of interest
for(i in levels(dat2$Size.class)){
  print(mean(dat2[dat2$Size.class == i,"Mass"]))
}


levels(dat2$Size.class)
class(dat2$Size.class)

# more complex:
# define a new vector or data frame outside the for loop first
new_vector = c() # it's empty
# also define a counter
x = 1

for(i in levels(dat2$Size.class)){
  new_vector[x] = mean(dat2[dat2$Size.class == i,"Mass"])
  x = x+1 # add 1 to the counter (this will change the element of new_vector we access each loop)
}
  
#check it
new_vector



# PUT THIS TOGETHER WITH THE LEVELS OF OUR FACTOR SO WE HAVE A NEW DATA FRAME:
# FIRST COLUMN WILL BE THE FACTOR LEVELS....
# SECOND COLUMN WILL BE NAMED "MEAN" AND WILL BE VALUES FROM  new_vector

#fill it in
size_class_mean_mass = data.frame(Size_Class = levels(dat$Size.class), Mean = new_vector)
size_class_mean_mass

############ YOUR HOMEWORK ASSIGNMENT ##############
##Objects
setwd("C:/Users/Aly/Desktop/GIT/Data_Course/data/")
dat = read.csv("thatch_ant.csv")
a = which(dat$Headwidth == "41mm")
dat$Headwidth[a] = "41.000"
dat$Headwidth = as.numeric(dat$Headwidth)
which(dat$Headwidth == NA)
bad = which(dat$Headwidth == 1)
dat$Headwidth[bad] = NA
dat2 = na.omit(dat)

# 1.  Make a scatterplot of headwidth vs mass. See if you can get the points to be colored by "Colony"
install.packages("RColorBrewer")
library(RColorBrewer)
colfact = as.factor(dat2$Colony)
nlevels(colfact) #I just wanted to see how many levels there were (ie colonies) for coloration (11 levels = 11 colonies = 11 colors)
display.brewer.all()
cols <- brewer.pal(n=11, name = "Spectral")
colsColny <- cols[dat2$Colony]
plot(dat2$Headwidth, dat2$Mass, main = "Thatch Ant Headwidth vs Mass", xlab = "Headwidth (mm)", ylab = "Mass (unit)", col = colsColny, pch = 20)
legend("topleft",legend = paste(rep(c("Colony"), times = 11), c(1:11)), col = cols, pch = 20)

## I know there is extra stuff that wasn't required for the task, but I found some 
## helpful things like the legend command and color packages, and figured it might be a good place to learn and apply. 

# 2.  Write the code to save it (with meaningful labels) as a jpeg file

jpeg(filename = "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Thatch_Ant_Plot.jpeg", width = 700, height = 550, quality = 100)
plot(dat2$Headwidth, dat2$Mass, main = "Thatch Ant Headwidth vs Mass", xlab = "Headwidth (mm)", ylab = "Mass (unit)", col = colsColny, pch = 20)
legend("topleft",legend = paste(rep(c("Colony"), times = 11), c(1:11)), col = cols, pch = 20)
dev.off()

# 3.  Subset the thatch ant data set to only include ants from colony 1 and colony 2

dat2subset = dat2[dat2$Colony <= 2,] #Shorter/convenient for specific task, but limited
##OR
dat2subset2 = subset(dat2, Colony == 1 | Colony == 2) #More appropriate for broader application

## I wasn't sure if you wanted us to use the for loop for this task, seeing as it is what we covered, but when
## I was trying to set it up (maybe incorrectly) the command I was using could stand alone (see above) and the extra
## script for the for loop seemed superfluous. 


# 4.  Write code to save this new subset as a .csv file
write.csv(dat2subset, "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Thatch_Ant_Subset.csv")

# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas
      # I should be able to run your R script and get all the plots created and saved, etc.
