library(dplyr)
library(rBLAST)

download.file('https://zenodo.org/record/3686052/files/blasthits2.tab.gz?download=1',overwrite=T, destfile = 'blasthits2.tab.gz')
system('gunzip blasthits2.tab.gz')
blasthits = read.table('blasthits2.tab')
head(blasthits)

#Split the table into a list of tables grouped by the Read ID
listhits = blasthits[1:1000,] %>% #Only does it for the first 1000 rows
  group_by(QueryID) %>% 
  group_split()  

# find the column that doesn't change
# taxonomy column names:
taxnames = c('superkingdom', 'phylum', 'order', 'family', 'genus', 'species')
shortnames = apply(listhits[[1]][,taxnames], 2, unique) #apply: apply to every element in a table
countshnames = sapply(shortnames, length) #sapply: apply to every element in a list
shortnames #print shortnames, shortnames is now a list which is why it looks so weird


#Which level is the highest group that is unique
lastuni = tail(names(countshnames[countshnames==1]), n=1)#tail: cuts off the last 6 elements of what you give it (shnames==1), and can be specified with how much of the tail we want (n=1).
lastuni

listhits[[1]][1,lastuni]

#make it a function
plus <- function(x,y){
  z=x+y
  return(z)
  
  
}

plus(1,1)


lca <- function(x){
 #x a subset of BLAST hits for one read
    taxnames = c('superkingdom', 'phylum', 'order', 'family', 'genus', 'species')
    shortnames = apply(x, 2, unique)
    countshnames = sapply(shortnames, length)
    lastuni = tail(names(countshnames[countshnames==1]), n=1)
    nombre = as.data.frame(x[1,which(colnames(x) == lastuni)])
    ret = x %>% 
      mutate(last_common = as.character(nombre[[1]])) %>%
      mutate(level_lca = lastuni)
    return(ret)
}

test1=lca(listhits[[1]])
test1$last_common


listhits =blasthits[1:10000,] %>%
  group_by(QueryID) %>%
  group_modify(~ lca(.x))

unique(listhits$last_common)
