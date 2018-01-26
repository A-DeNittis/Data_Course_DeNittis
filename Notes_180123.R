setwd("C:/Users/Aly/Desktop/Data_Course/data/")
list.files()
list.files()[1:5]
df = read.csv("Fake_grade_data.csv")

names(df)

df$Total = rowSums(df[,'X'])
                   
row.names(df) <- df$Student                   
df = df[,-c(1,2)]
df$Total = rowSums(df[,1:15])

A = df[df$Total >= 700,]

?arrange
?order
?sort

A[order(A$Total),]
sort(A$Total)

sample(df$Total)
?sample

set.seed(10)

#Recommend rearranging based on order command, completely comfortable using square brackets, etc.

