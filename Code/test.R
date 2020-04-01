library(Biostrings)
library(taxonomizr)
library(ggplot2)
library(ShortRead)
library(dplyr)
library(stringi)
library(forcats)
library(rBLAST)
library(multidplyr)
library(cowplot)

#devtools::install_github("tidyverse/multidplyr"){force:true}
#devtools::install_github("mhahsler/rBLAST")

# Download SRA File:
srr=c('SRR7627064')
system(paste('fastq-dump', srr, sep=' '))


# Read taxonomy database
taxaNodes<-read.nodes.sql("/projectnb/ct-shbioinf/taxonomy/data/nodes.dmp")
taxaNames<-read.names.sql("/projectnb/ct-shbioinf/taxonomy/data/names.dmp")


# read fastq
dna = readFastq(paste(srr, '.fastq',sep=''))
reads = sread(dna)
qscores = quality(dna) 

# plot readlength
widths = as.data.frame(reads@ranges@width)
(widthplot <- ggplot(widths) +
    geom_histogram(aes(x=reads@ranges@width), binwidth = 10) + 
    theme_linedraw() + 
    xlab('Read Length (bp)') +
    xlim(0,2000) +
    ggtitle('Read length distribution for 550bp amplicon'))
ggsave(widthplot, file='readlengths.png')

# plot qscores
numqscores = as(qscores, "matrix") # converts to numeric scores automatically
avgscores = apply(numqscores, 1, mean, na.rm=TRUE) #apply the function mean() across 
avgscores = as.data.frame(avgscores)
(qscores = ggplot(avgscores) +
    geom_histogram(aes(x=avgscores), binwidth=0.2) +
    theme_linedraw() +
    xlab('Quality Score') +
    ggtitle('Per Read Average Quality'))
ggsave(qscores, file='quality.png')


## blast
bl <- blast(db="/projectnb/ct-shbioinf/blast/nt.fa")
cl <- predict(bl, reads, BLAST_args = '-num_threads 8 -evalue 1e-50')
accid = as.character(cl$SubjectID) # accession IDs of BLAST hits

# Plot results

#takes accession number and gets the taxonomic ID
ids<-accessionToTaxa(accid, '/projectnb/ct-shbioinf/taxonomy/data/accessionTaxa.sql')
#taxlist displays the taxonomic names from each ID #
taxlist=getTaxonomy(ids, taxaNodes, taxaNames)

cltax=cbind(cl,taxlist)


lca2 = function(x) {
  require(dplyr)
  taxnames = c('superkingdom', 'phylum', 'order', 'family', 'genus', 'species')
  x = x %>% filter(!is.na(superkingdom)) %>% filter(superkingdom != 'Bacteria') # need to deal with this more generally
  shortnames = apply(x[,taxnames], 2, unique)
  countshnames = sapply(shortnames, length)
  numcount = countshnames==1
  lastuni = tail(names(shortnames[numcount==T]), n=1)
  nombre = as.data.frame(x[1,which(colnames(x) == lastuni)])
  newtax <- as.list(ifelse(countshnames==1,shortnames,NA))
  
  ret = x %>% 
    mutate(last_common = as.character(nombre[[1]])) %>%
    mutate(level_lca = lastuni) %>%
    mutate(superkingdom = newtax$superkingdom) %>%
    mutate(phylum = newtax$phylum) %>%
    mutate(class = newtax$class) %>%
    mutate(order = newtax$order) %>%
    mutate(family = newtax$family) %>%
    mutate(genus = newtax$genus) %>%
    mutate(species = newtax$species)
  return(ret)
}

blasthits = cltax

cluster <- new_cluster(8)
cluster_library(cluster, 'dplyr')
cluster_copy(cluster, 'lca2')

#split groups across multiple CPU cores
prepare = blasthits %>%
  group_by(QueryID) %>%
  partition(cluster) %>%  #split groups into cluster units
  do({lca2(.)}) %>%
  collect()

#count reads matching species:
spcount = prepare %>%
  group_by(QueryID) %>%
  slice(1) %>%
  group_by(species) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(species))


#count reads matching to genera:
gencount = prepare %>%
  group_by(QueryID) %>%
  slice(1) %>%
  group_by(genus) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(genus))

#count reads matching to families
famcount = prepare %>%
  group_by(QueryID) %>%
  slice(1) %>%
  group_by(family) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(family))


(p1 = ggplot(spcount) +
    geom_col(aes(x=fct_reorder(species, count, .desc=T), y=count)) + 
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90)) +
    xlab('Species')
)

(p2 = ggplot(gencount) +
    geom_col(aes(x=fct_reorder(genus, count, .desc=T), y=count)) + 
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90)) +
    xlab('Genus')
)

(p3 = ggplot(famcount) +
    geom_col(aes(x=fct_reorder(family, count, .desc=T), y=count)) + 
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90)) +
    xlab('Family')
)

gr = plot_grid(p1, p2, p3, ncol=1, nrow=3)

ggsave(gr, file='grid_taxonomy.png', height = 9, width = 5, dpi=500)