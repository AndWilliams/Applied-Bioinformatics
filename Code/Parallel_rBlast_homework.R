library(parallel)
library(rBLAST)
library(ShortRead)


blastdb = "/projectnb/ct-shbioinf/blast/nt.fa"

srr=c('SRR11043494')

system(paste('fastq-dump', srr, sep=' '))


dna = readFastq('.', pattern=srr)
reads = sread(dna, id=id(dna))
reads = reads[1:1000]
bl <- blast(db=blastdb, type='blastn')

# linear
p = proc.time()
cl <- predict(bl, reads, BLAST_args = ' -evalue 1e-100') #blastn with 4 threads
proc.time() - p


# with num_threads 2
p = proc.time()
cl <- predict(bl, reads, BLAST_args = '-num_threads 2 -evalue 1e-100') #blastn with 4 threads
proc.time() - p

#Make it into a function
blastit = function(x) {
  return(predict(bl, x,  BLAST_args= '-evalue 1e-100'))
}


nclus = 2
p = proc.time()
cl = makeCluster(nclus, type = 'FORK')
splits = clusterSplit(cl, reads)
p_works = parLapply(cl, splits, blastit)
stopCluster(cl)
proc.time() - p
