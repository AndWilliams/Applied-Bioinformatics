#!/usr/bin/R

library(ggplot2)
library(rBLAST)
library(dplyr)
library(forcats)
library(ShortRead)
library(taxonomizr)

#Choose your SRR file
srr=c('SRR11043497')
system(paste('fastq-dump', srr, sep=' '))

#read Fastq
dna = readFastq('.', pattern=srr)

#Parse data
reads= sread(dna, id=id(dna))
qscores=quality(dna)

#Blast data
bl <- blast(db="/projectnb/ct-shbioinf/blast/nt.fa")
cl <- predict(bl, reads, BLAST_args = '-num_threads 12 -evalue 1e-100')

#Taxonomizes
accid = as.character(cl$SubjectID)
taxaNodes<-read.nodes.sql("/projectnb/ct-shbioinf/taxonomy/data/nodes.dmp")
taxaNames<-read.names.sql("/projectnb/ct-shbioinf/taxonomy/data/names.dmp")
ids<-accessionToTaxa(accid,'/projectnb/ct-shbioinf/taxonomy/data/accessionTaxa.sql')
taxlist=getTaxonomy(ids, taxaNodes, taxaNames)
cltax=cbind(cl,taxlist)

#visualize 
cltop = cltax %>% 
  group_by(QueryID) %>% 
  top_n(n=1, wt=Bits)

(ggplot(data=cltop) +
    geom_bar(aes(x=fct_infreq(genus))) +
    theme_minimal() +
    theme(    
      axis.text.x  = element_text(angle = 45, hjust=1)
    ) +
    xlab('')
  
)
