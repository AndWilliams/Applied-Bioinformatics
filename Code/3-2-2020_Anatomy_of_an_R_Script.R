#!/usr/bin/R

#Define all libraries at the top
library(ggplot2) #etc...

#Create variables
v1 = seq(1, 10)
v2 = seq(10, 100, by=10)


#Do calculations or methods
v3 = v2 * v1
res = as.data.frame(cbind(v1, v2, v3))

#generate files and output
plo = ggplot(data=res) +
  geom_point(aes(x=v1, y=v3)) +
  geom_smooth(aes(x=v1, y=v3)) 
ggsave('plot1.pdf', plo)