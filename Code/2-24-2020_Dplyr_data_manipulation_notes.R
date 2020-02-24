library(dplyr)

#data manipulation in dplyr

# max x
stuff %>%
  group_by(gp) %>%
  summarize(max.x=max(x))

# count subgroups
stuff %>% 
  group_by(gp, sub) %>%
  summarize(count=n())

#working with BLAST hits
#read BLAST hits file: 
blasthits = read.table('blasthits2.tab')
dim(blasthits)

#dplyr: selecting columns and filtering
#This table is very large and maybe we don’t want all of the columns.
colnames(blasthits)
#Trim the table to only use QueryID, SubjectID, Perc.Ident, Alignment.Length, E, Bits, family, genus.
sele.hits = select(blasthits, QueryID, SubjectID, Perc.Ident, Alignment.Length, E, Bits, family, genus)
colnames(sele.hits)
#OR Filter the table by match criteria (e.g., E, Bits, or Perc.Ident)
top.hits = filter(blasthits, Bits>350)
nrow(top.hits)
nrow(blasthits)
#dplyr: group and summarize 
#For example, we might want to know the number of times that a particular Genus is identified in the BLAST hits. Which might look like:
groupgen = group_by(blasthits, genus) 
topgen = summarize(groupgen, count = n())
head(arrange(topgen, desc(count))) # PRINT JUST THE HIGHEST COUNTS
#For example, in the BLAST results we want to know what the top matches are for each sequence read. That can be accomplished by code like:
groupbyread = group_by(blasthits, QueryID)
topbygroup = filter(groupbyread, any(Bits>350))

#dplyr: pipe things together
#This is accomplished with the pipe operator “%>%”. The pipe is code that redirects the output of one command directly into the primary input of another.
#For example, the last example where we grouped by QueryID and then filtered by Bit score. Well, that could be done in a single line with:
topbygroup = blasthits %>% group_by(QueryID) %>% filter(any(Bits>350))
#There is not limit to the complexity of your pipe commands. If you wanted to take the filtered dataset above and go further to summarize the counts by genus that would look like:
topgenera = blasthits %>% #start pipe by hooking it up to blasthits table
  group_by(QueryID) %>% #Group on query ID
  filter(any(Bits>500)) %>% #filter for Bit score
  group_by(genus) %>% #redo grouping for genus column now
  summarize(count=n()) %>% #summarize by counting rows in each genus 
  arrange(desc(count))

#What are the best matches by E value
#That’s still probably not the count we really want to show. Note that there are more hits to Callitriche in this table than there are reads in the dataset. We should get only the best match for each read.
topgenera = blasthits %>% 
  group_by(QueryID) %>% 
  top_n(n=1, wt=E) %>% #top by E value this time
  group_by(genus) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
topgenera #prints out results