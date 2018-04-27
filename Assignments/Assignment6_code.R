setwd ("C:/Users/Aly/Desktop/GIT/Data_Course/assignments/Assignment_6/")
list.files()

#1. Convert these fastq files to fasta format and save them as separate files.
GIT Term: $ alias fqtofa='sed -n '\''1~4s/^@/>/p;2~4p'\'''  ' 
GIT Term: $ for fn in *.fastq; do fqtofa $fn > $fn.fasta;done

#2. Count the number of sequences in each fasta file.

GIT Term: $ grep -c "^>" *.fasta

#3. Trim each fastq file to the first 150 base pairs and truncate/filter each read 
#at the default quality score of 2. Make sure to remove any reads with uncalled bases (N)

fq.files = dir(path = getwd(), full.names = TRUE, pattern = ".fastq")
filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")
dir.create(file.path(getwd(), "filtered"))


for(i in filt.files){
  fastqFilter(fn = i,fout = paste0(getwd(),"/filtered/",i,".filt"),
              truncLen = 150)
}

#4. Using the error rates in these filtered reads, use dada2 to correct mis-called bases.

library(dada2)
setwd(file.path(paste0(getwd(),"/filtered")))
filtered = dir(path = getwd(), pattern = ".filt")

dr = derepFastq(filtered)

err = learnErrors(dr)

plotErrors(err)

clean = dada(dr,err)


#5. Create a "species" abundance table

SeqTable = makeSequenceTable(clean)

#6. Assign taxonomy to these reads based on the fungal database provided "sh_general_release_dynamic_10.10.2017.fasta.gz" 
#(the rdp_train_set won't work since these are fungal, not bacterial reads...we need a fungal database)
#If everything is working correctly, this step will take quite a while to run...~15 minutes on my machine.

taxonomy = assignTaxonomy(SeqTable, refFasta = "C:/Users/Aly/Desktop/GIT/Data_Course/assignments/Assignment_6/fastas/sh_general_release_dynamic_10.10.2017.fasta")

# 7. Change the column names in your species abundance table to the assigned taxonomy names.

taxa.print <- taxonomy # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
taxa.print

st = as.data.frame(SeqTable)
taxo = as.data.frame(taxonomy)
length(which(names(st) %in% row.names(taxo)))

names(st) <- paste0(taxo$Family,"_",taxo$Genus)


#8. Export this species abundance table with propoerly assigned names as a csv file.

write.csv(st, "C:/Users/Aly/Desktop/GIT/Data_Course_DeNittis/Assignments/Assignment6_abundancetable.csv")
