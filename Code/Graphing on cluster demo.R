library(rBLAST)
library(ShortRead)
library(taxonomizr)
library(dplyr)
library(ggplot2)
library(forcats)


rapid_test = readFastq('/projectnb/ct-shbioinf/awillia5', pattern='SRR11043497.fastq')
reads = sread(rapid_test) 
idstr = id(rapid_test) 
qscores = quality(rapid_test)

widths = as.data.frame(reads@ranges@width)
head(widths) #reads out the length of the reads inside the file

numqscores = as(qscores, "matrix") #converts to numeric scores
avgscores = apply(numqscores, 1, mean, na.rm=TRUE) #applies the average score to each column  
length(avgscores)
avgscores = as.data.frame(avgscores)
colnames(avgscores) #changing the name of the column  

ggplot(data=widths)+
  geom_histogram(aes(x=reads@ranges@width)) +
  ggtitle("Read Lengths vs. Quality Scores") +
  xlab("Read Lengths") +
  ylab("Quality Scores")
