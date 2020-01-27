library(ggplot2)

census = read.csv('https://raw.githubusercontent.com/rsh249/applied_bioinformatics2020/master/data/census.csv')

head(census)## first couple lines
nrow(census)##shows how many lines are in the file

ggplot(data = census) +
  geom_point(aes(x = Citizen, y = TotalPop))

##single parameter graphs 
#Histogram
ggplot(data=census) +
  geom_histogram(aes(x=Citizen), binwidth=100)+
  xlim(0,10000) ##limits the x axis 

##geom density
ggplot(data=census) +
  geom_density(aes(x=Citizen)) +
  xlim(c(0,10000))

#geom bar is used for discrete variables
#barplot of states
ggplot(data=census) +
  geom_bar(aes(x=State))

library(forcats)
ggplot(data=census) +
  geom_bar(aes(x=fct_infreq(State))) +
  xlab('') +
  theme(axis.text.x  = element_text(angle=90))

#plotting mean commute time
ggplot(data=census) +
  geom_density(aes(x=MeanCommute))

#graphs for two parameters
ggplot(data=census) + 
  geom_point(aes(x=IncomePerCap, y=Poverty))

#adding a line of best fit
ggplot(data=census) + 
  geom_point(aes(x=IncomePerCap, y=Poverty), alpha=0.1) +
  geom_smooth(aes(x=IncomePerCap, y=Poverty))

#plotting with a linear model
ggplot(data=census) + 
  geom_point(aes(x=IncomePerCap, y=Poverty), alpha=0.1) +
  geom_smooth(aes(x=IncomePerCap, y=Poverty), method='lm')
  
#contour and density plots
ggplot(data=census) + 
  geom_hex(aes(x=IncomePerCap, y=Poverty)) 

#changing appearance
census2=census[census$State=='Massachusetts',]

ggplot(data=census2) + 
  geom_point(aes(x=IncomePerCap, y=Poverty, col='red'))

#assigns random arbitrary numbers to the points of data
ggplot(data=census2) + 
  geom_point(aes(x=IncomePerCap, y=Poverty, col=County), alpha=0.1)

#Themes
ggplot(data=census2) + 
  geom_point(aes(x=IncomePerCap, y=Poverty, col=County), alpha=0.1) +
  theme_bw()

#theme minimal
ggplot(data=census2) + 
  geom_point(aes(x=IncomePerCap, y=Poverty, col=County), alpha=0.1) +
  theme_minimal()

#theme_classic, the default setting

#theme void, not really useful
ggplot(data=census2) + 
  geom_point(aes(x=IncomePerCap, y=Poverty, col=County), alpha=0.1) +
  theme_void()