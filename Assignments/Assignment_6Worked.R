### Assignment 6 ###
####################

library(ShortRead)
library(tidyr)
library(stringr)
library(msa)
library(seqinr)
library(dada2)
library(vegan)
setwd("C:/Users/Aly/Desktop/GIT/Data_Course/assignments/Assignment_6/")
list.files()

# Convert these fastq files to fasta format and save them as separate files.

GIT Term: $ alias fqtofa='sed -n '\''1~4s/^@/>/p;2~4p'\'''  ' 
GIT Term: $ for fn in *.fastq; do fqtofa $fn > $fn.fasta;done

fq.files = dir(path = getwd(), full.names = TRUE, pattern = ".fastq")

fq.1 = readFastq(fq.files[1])
fq.2 = readFastq(fq.files[2])

writeFasta(fq.1,"fq1.fasta")
writeFasta(fq.2,"fq2.fasta")

# Count the number of sequences in each fasta file.

GIT Term: $ grep -c "^>" *.fasta # prints counts for each file


# Trim each fastq file to the first 150 base pairs and truncate/filter each read at the default quality score of 2. Make sure to remove any reads with uncalled bases (N)


fq.files = dir(path = getwd(), full.names = TRUE, pattern = ".fastq")
filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")
dir.create(file.path(getwd(), "filtered"))


for(i in filt.files){
  fastqFilter(fn = i,fout = paste0(getwd(),"/filtered/",i,".filt"),
              truncLen = 150)
}

# Using the error rates in these filtered reads, use dada2 to correct mis-called bases.

setwd(file.path(paste0(getwd(),"/filtered")))
filtered = dir(path = getwd(), pattern = ".filt")

dr = derepFastq(filtered)

err = learnErrors(dr)

plotErrors(err)

clean = dada(dr,err)


# Create a "species" abundance table

SeqTable = makeSequenceTable(clean)

# Assign taxonomy to these reads based on the fungal database provided "sh_general_release_dynamic_10.10.2017.fasta.gz" (the rdp_train_set won't work since these are fungal, not bacterial reads...we need a fungal database)
# If everything is working correctly, this step will take quite a while to run...~15 minutes on my machine.


taxonomy = assignTaxonomy(SeqTable, refFasta = "C:/Users/Aly/Desktop/GIT/Data_Course/assignments/Assignment_6/fastas/sh_general_release_dynamic_10.10.2017.fasta")

taxa.print <- taxonomy # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
taxa.print

which(names(as.data.frame(SeqTable)) %in% row.names(as.data.frame(taxonomy)))
RSV = as.data.frame(SeqTable)

taxa = as.data.frame(taxonomy)
View(RSV)
names(RSV) <- paste0(taxa$Family,"_",taxa$Genus)
View(RSV)


write.csv(RSV,file = "RSV_Table_w_taxonomy.csv")



7. Change the column names in your species abundance table to the assigned taxonomy names.



8. Export this species abundance table with propoerly assigned names as a csv file.



Assingment 6 in Git

-1- # convert fastq to fasta #
  gunzp *.gz #unzips zll ziped files in the directory

alias fqtofa='sed -n '\''1~4s/^@/>/p;2~4p'\''(add 2 more, messes up r code)

#set alias for converting fq to fa files

for fn in *.fastq; do fqtofa $fn > $fn.fasta; done 
#uses alias to convert all the .fastq files to .fasta
#new files are labeled .fastq.fasta; were still converted to fasta format

example of individual conversion
sed -n '1~4s/^@/>/p;2~4p' Distromium2.fastq > Distromium2_1.fa
alias fqtofa= 'sed -n '1~4s/^@/>/p;2~4p' file.fq > file.fa'

- all fastq files were ziped
gzip *.fastq

-2- # number of sequences in each fasta file #
grep -c "^>" *.fa

Assingment 6 in R-studio

-3- # dada2 trim and filter #
filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")
dir.create(file.path(getwd(),"filtered"))
file.path(getwd(),"filtered/",i)

for(i in filt.files){ 
fastqFilter(fn=i,fout=paste0(getwd(),"/filtered/",i,".filt"),
truncLen = 150)}
#filters throught and trims the sequences. Filters with quality score of 
2 and removes any uncalled bases (N) by defualt.

-4- # correct miscalled bases #
setwd(file.path(paste0(getwd(),"/filtered")))
filtered = dir(path = getwd(), pattern = ".filt")

dr = derepFastq(filtered) #dereplicate the data
err = learnErrors(dr) #predicts the error in the reads
plotErrors(err) #shows the frequence of a base changing to another base
clean = dada(dr,err)
#changes the errors to what they should be based on the predicted errors

-6- # assigning taxonomy #
taxonomy = assignTaxonomy(SeqTable, refFasta = "../fastas/sh_general_release_dynamic_10.10.2017.fasta")
#Assigns taxonomy using a .fasta file with name information

-5,7- # abundance table and changing names #
taxa.print <- taxonomy # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
taxa.print

which(names(as.data.frame(SeqTable)) %in% row.names(as.data.frame(taxonomy)))
RSV = as.data.frame(SeqTable)

taxa = as.data.frame(taxonomy)
View(RSV)
names(RSV) <- paste0(taxa$Family,"_",taxa$Genus)
View(RSV)

-8- # write .csv file #
write.csv(RSV,"D:/desktop/R_Analysis_Class/Data_Course_Blain/assignments/assignment_6/abundance_table.csv")


# Review of vegan
setwd("C:/Users/Aly/Desktop/GIT/Data_Course/code_examples/Vegan_Example/")
meta = read.delim("vegan_example_metadata.tsv", sep = "\t")
otu = read.delim("vegan_example_otu_table.tsv", sep = "\t")
library(vegan)

#look at OTU and we see that row names have a column name as X
otu = read.delim("vegan_example_otu_table.tsv", sep = "\t", row.names = 1)

names(otu)
# vegan does not like characters "tax_name" needs to be removed
otu$Tax_Name
# pull it off the table and save for later
# make a vector to save for later, we do not want to delete it completely
tax = otu$Tax_Name
otu$Tax_Name = NULL

diversity(otu)
# vegan wants sites to be rows and species to be columns. We need to transpose
t_otu = as.data.frame(t(otu)) # need to used the as.data.frame otherwise "t" turns it into a matrix
diversity(t_otu)
# look at meta data to see where we are coming from. Lets look at ecosystem to see if there is significant
# in marine vs terrstrial

levels(meta$Ecosystem) # character 
# we want to make sure all these rows are in the same order because they are similar information. 
# are these identical?
meta$Sample_ID # factor
identical(row.names(t_otu), as.character(meta$Sample_ID))

# want to make the matrix t_otu into a response variable, do a PermANOVA using adonis
adonis(t_otu ~ meta$Ecosystem) # metric calculation
# tells us ecosystem is important, now we want to visualize it
NMDS = metaMDS(t_otu) # non metric multi dimensional scaling
#take a look, what we want for plotting it "points"
NMDS$points

# pull each point out to its own
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]

plot(MDS1, MDS2)
# make it tdy with a data frame
df = data.frame(MDS1 = MDS1, MDS2 = MDS2, Ecosystem = meta$Ecosystem)
library(ggplot2)
ggplot(df, aes(x=MDS1, y=MDS2, col = Ecosystem)) + geom_point() + stat_ellipse()
# ellipse is the 95% confidence interval


adonis(t_otu ~ meta$Ecosystem*meta$Host)
df = data.frame(MDS1 = MDS1, MDS2 = MDS2, Ecosystem = meta$Ecosystem, Host = meta$Host)
ggplot(df, aes(x=MDS1, y=MDS2, col = Host)) + geom_point() + stat_ellipse()
# we dont have a large enough number of hosts replication to run the test. 



heatmap(t_otu)
# t_otu is a data frame not a matrix
# because this is base R we have to change the ecosystem to colors
library(plyr)
colors = as.character(mapvalues(meta$Ecosystem, from = c("Marine", "Terrestrial"), to = c("Blue", "Red")))

heatmap(as.matrix(t(t_otu)), ColSideColors = colors, col = gray.colors(100))


# turn it into a R Mark down
