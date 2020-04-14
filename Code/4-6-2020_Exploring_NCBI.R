library(rentrez)
library(ShortRead)
library(Biostrings)
library(xml2)
library(dplyr)
library(ggplot2)


# list all ncbi databases
entrez_dbs()


#find searchable terms in one DB
entrez_db_searchable("sra")

r_search <- entrez_search(db="sra", term="PRJNA605442[GPRJ]", retmax=60)
print(r_search)

id_fetch <- entrez_fetch(db="sra", id="10232377", rettype = 'xml')

doc <- read_xml(id_fetch)
SRAFile <- doc %>% xml_find_all("//RUN_SET") %>% xml_find_all("//SRAFile")


#final product
r_search <- entrez_search(db='sra', term="PRJNA605442[GPRJ]", retmax=60)
id_fetch <- entrez_fetch(db="sra", id=r_search$ids, rettype = 'xml')
doc <- read_xml(id_fetch)
SRAFile <- doc %>% 
  xml_find_all("//RUN_SET") %>% 
  xml_find_all("//SRAFile") %>% 
  xml_attr('url') 

get_one = SRAFile[grepl("SRR11043468", SRAFile)]
download.file(SRAFile[1], 'test.fastq')
dna = readFastq('.', pattern='test.fastq') ## On to BLAST analysis >>>

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