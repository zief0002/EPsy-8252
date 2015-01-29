
###################################################
### Create vectors
###################################################

A = c(3, 1, 0)
B = c(4, -3, 2)



###################################################
### Dimension of a vector
###################################################

length(A) 
length(B) 



###################################################
### Vector operations
###################################################

#Addition/subtraction
A + B 
A - B

#Scaling
4 * A 
1/2 * B

#Linear combination
3 * A + 2 * B 



###################################################
### Dot product between A and B
###################################################

sum(A * B) 

#length of vector A
sqrt(sum(A * A)) 

#length of vector B
sqrt(sum(B * B))



###################################################
### Compute the angle between two vectors
###################################################

#Compute cos(theta)
sum(A * B)/ (sqrt(sum(A * A)) * sqrt(sum(B * B)))  

#Compute theta and convert to degrees
acos(0.5284982) * 180 / pi 

