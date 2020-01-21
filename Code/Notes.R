# assign a value to variable x
x=2

# print out our variable
print(x)

#Ask what the data type is inside of the variable
class(x)

#vectors: list of variables, basically just arrays from Java
z=seq(10:1) # A function to create a vector
class(z)
print(z)

#index value
print(z[4])
z[4] = 'four'


#call more than one index
print(z[1]) #first index of vector
print(z[1:5]) #print all 5
print(z[c(1,3,5)]) #print only index 1, 3, and 5

#matrixs are just 2 dimensional arrays
y= matrix(nrow=5, ncol=5)#create a 5x5 matrix
print(y) #shows entire matrix
class(y) #just says matrix, doesnt tell you whats actually in it

y[1,1]=5 #change a cell value

#changing an entire column or row
y[,1]= 5 # put data in column 1

print(y[3,]) #only prints row 3


#reading and writing files
file = 'https://raw.githubusercontent.com/rsh249/applied_bioinformatics2020/master/data/mtcars.csv' #URL to text file with data

cars = read.table(file, header=T, sep = ',') # Read a comma separated values file
head(cars)

#writing files
write.table(cars, file='mtcars.tab')


