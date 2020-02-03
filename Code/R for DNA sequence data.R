library(ShortRead)
library(ggplot2)

dir.create("data", showWarnings = FALSE)
download.file("https://raw.githubusercontent.com/rsh249/applied_bioinformatics2020/master/data/all.fastq", "data/rapid_test.fastq")
rapid_test = readFastq('data', pattern='rapid_test.fastq')

# ShortRead Functions for looking at fastq data
reads = sread(rapid_test) # the set of sequence data
idstr = id(rapid_test) # id numbers
qscores = quality(rapid_test) # sequence quality strings

#Assess read lengths
widths= as.data.frame(reads@ranges@width)

ggplot(data=widths)+
  geom_histogram(aes(x=reads@ranges@width))


#sread() parses fastq object for the DNA sequences 
reads = sread(rapid_test)
class(reads)
length(reads)

#print(reads@ranges) #how each read is mapped in the object
#print(reads@pool) #looks like the entire memory object of sequences
widths = as.data.frame(reads@ranges@width)

(widthplot <- ggplot(widths) +
    geom_histogram(aes(x=reads@ranges@width), binwidth = 100) + 
    theme_linedraw() + 
    xlab('Read Length (bp)') +
    ggtitle('Not a great read length distribution'))

#q scores
numqscores = as(qscores, "matrix") # converts to numeric scores automatically, also "as(qscores, "matrix")" converts to a matrix
nrow(numqscores)
ncol(numqscores)
avgscores = apply(numqscores, 1, mean, na.rm=TRUE) #apply the function mean() (could be any statistical function like stdev) across all rows (argument '1') of the matrix and ignore "NA" values


#convert toa data frame
avgscores = as.data.frame(avgscores)
colnames(avgscores) #need the column name for ggplot

geom_histogram(aes(x=avgscores), binwidth=0.2) +
  theme_linedraw() +
  xlab('Quality Score') +
  ggtitle('Per Read Average Quality')