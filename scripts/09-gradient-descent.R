#####################################
# Load Libraries
#####################################

library(ggplot2)
library(dplyr)



#####################################
# Deviance function
#####################################

dev = function(b0) {
	-2 * (-5/2 * log(2 * pi * 12.15362) - 1/(2*12.15362) * ( (12 - b0)^2 + (8 - b0)^2 + (16.26 - b0)^2 + 
	(13.65 - b0)^2 + (8.5 - b0)^2 ) )
	}



#####################################
# Compute derivative
#####################################

g = expression(-2 * (-5/2 * log(2 * pi * 12.15362) - 1/(2*12.15362) * ( (12 - b0)^2 + (8 - b0)^2 + (16.26 - b0)^2 + 
  (13.65 - b0)^2 + (8.5 - b0)^2 ) ) )

D(g, "b0")

# -(2 * (1/(2 * 12.15362) * (2 * (8.5 - b0) + (2 * (13.65 - b0) + 
#    (2 * (16.26 - b0) + (2 * (8 - b0) + 2 * (12 - b0)))))))



#####################################
# Write function for computing derivative
#####################################

dev_prime = function(b0){
  -(2 * (1/(2 * 12.15362) * (2 * (8.5 - b0) + (2 * (13.65 - b0) + 
    (2 * (16.26 - b0) + (2 * (8 - b0) + 2 * (12 - b0)))))))
}  

## Try function
b00 = 0
alpha = .1

dev_prime(b00)



#####################################
# gradient descent
#####################################

for(i in 1:100){
  b01 = b00 - alpha * dev_prime(b00)
  print(b01)
  b00 = b01
}


#####################################
# gradient descent function
#####################################

grad_desc = function(b00, alpha = 0.1, tol = .01){

  # First iteration
  b01 = b00 - alpha * dev_prime(b00)
  diff = b01 - b00
  i = 1
  print(c(i, b00, b01, diff))
  
  # Check tolerance and repeat if necessary
  while(diff > tol){
        i = i + 1
    #print(b01)
    b00 = b01
    b01 = b00 - alpha * dev_prime(b00)
    diff = b01 - b00
    print(c(i, b00, b01, diff))
    }
}

grad_desc(b00 = 0)



#####################################
# gradient descent function w/pretty print
#####################################

grad_desc = function(b00, alpha = 0.1, tol = .01){

  # First iteration
  b01 = b00 - alpha * dev_prime(b00)
  diff = b01 - b00
  iter = 1
  new = data.frame(iteration = iter, b00 = b00, b01 = b01, diff = diff)
  
  # Check tolerance and repeat if necessary
  while(diff > tol){
        iter = iter + 1
    #print(b01)
    b00 = b01
    b01 = b00 - alpha * dev_prime(b00)
    diff = b01 - b00
    new2 = data.frame(iteration = i, b00 = b00, b01 = b01, diff = diff)
    new = do.call(rbind, list(new, new2))
    }
   
   # Get the final estimate of b0
   x = paste("The estimate of b0 is ", round(b01, 3), ".", sep = "")

   # Output the data frame and the final estimate
   return(list(new, x)) 
}

# Run the function
grad_desc(b00 = 0)


