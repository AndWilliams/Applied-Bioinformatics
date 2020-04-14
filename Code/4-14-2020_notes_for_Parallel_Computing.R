#Parallel computing for R

#Test function to setup a sequence of numbers and check if their prime
do = seq(1, 50000)

# and a function:
is.prime <- function(num) {
  r = vector()
  for(n in 1:length(num)){
    if (num[n] == 2) {
      r = c(r, TRUE)
    } else if (any(num[n] %% 2:(num[n]-1) == 0)) {
      r = c(r, FALSE)
    } else { 
      r = c(r, TRUE)
    }
  }
  return(r)
}


#For loop

p = proc.time();
sq=vector()
for (d in 1:length(do)){sq[d] = is.prime(do[d])}
proc.time() - p; #These numbers tells us how long the job took

#For loop applying our prime number function
p = proc.time();
l_works = sapply(do, is.prime);
proc.time() - p; 


#Now do all of the above but with parallel 

library(parallel)

nclus = 4 #Number of cores to allocate to this job:
cl = makeCluster(nclus, type = 'FORK')
#Create cluster of size n
p = proc.time()#start timer
splits = clusterSplit(cl, do) #split job into smaller parts of equal size
p_works = parSapply(cl, do, is.prime) #Run parts across cluster and collect results
proc.time() - p #end timer
stopCluster(cl)


#Do the same thing with multidplyr instead
library(dplyr)
library(multidplyr)

#make a data frame
dplyr.do = do %>% as.data.frame() %>%
  rename(num = '.') %>% 
  group_by(num)


#without parallel:
p = proc.time()
prepare = dplyr.do %>% rowwise() %>% mutate(isit = is.prime(num))
proc.time() -p

#With multidplyr
cluster <- new_cluster(nclus)
cluster_library(cluster, 'dplyr')
cluster_copy(cluster, 'is.prime')
p = proc.time()

#split groups across multiple CPU cores
prepare = dplyr.do %>%
  partition(cluster) %>%  #split  into cluster units
  mutate(isit = is.prime(num)) %>%
  collect() %>%
  arrange(num)
proc.time() - p



#Theoretically a better way to run rBLAST
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