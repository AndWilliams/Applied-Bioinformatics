#R for BLAST
library(rBLAST)
library(ShortRead)
library(taxonomizr)
library(dplyr)
library(ggplot2)
library(forcats)

#install.packages("taxonomizr")
#devtools::install_github("mhahsler/rBLAST")

setwd('/projectnb/ct-shbioinf/awillia5')

Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/share/pkg.7/blast+/2.7.1/install/bin", sep=":"))
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/share/pkg.7/sratoolkit/2.9.2/install/bin/", sep=":"))


srr=c('SRR11043494')
system(paste('fastq-dump', srr, sep=' '))

dna = readFastq('.', pattern=srr)
#parse DNA sequences
reads= sread(dna, id=id(dna))
#parse quality scores
qscores=quality(dna)

bl <- blast(db="/projectnb/ct-shbioinf/blast/nt.fa")
cl <- predict(bl, reads, BLAST_args = '-num_threads 12 -evalue 1e-100')

accid = as.character(cl$SubjectID) # accession IDs of BLAST hits

#load the taxonomy database files 'nodes' and 'names'
taxaNodes<-read.nodes.sql("/projectnb/ct-shbioinf/taxonomy/data/nodes.dmp")

taxaNames<-read.names.sql("/projectnb/ct-shbioinf/taxonomy/data/names.dmp")

# Search the taxonomy by accession ID #

# takes accession number and gets the taxonomic ID
ids<-accessionToTaxa(accid,'/projectnb/ct-shbioinf/taxonomy/data/accessionTaxa.sql')

# displays the taxonomic names from each ID #
taxlist=getTaxonomy(ids, taxaNodes, taxaNames)

# Merge BLAST hits and taxonomy of each
cltax=cbind(cl,taxlist)

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
