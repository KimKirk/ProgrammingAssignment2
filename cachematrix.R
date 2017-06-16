## makeCacheMatrix() holds several functions used to get and set matrix and inverse matrix
## cacheSolve() uses get and set functions from makeCacheMatrix() to get inverse matrix or calculate and set inverse matrix


## gets and sets matrix and creates a list to access those getter and setter functions by name

makeCacheMatrix <- function(x = numeric()) {
    ## holds inversed matrix, instantiates to null
    inverse_matrix <- NULL 
  
    ## sets the variables that hold the matrix and inversed matrix
    setMatrix <- function(y) { 
        ## assigns input to "x" variable (located inside of parent environment)
        x <<- y 
    
        ## instantiates "inversed_matrix" variable to null (located inside of parent environment)
        inverse_matrix <<- NULL 
  }
  
    ## gets the matrix
    getMatrix <- function() x  
  
    ## sets the variable "inversed_matrix" to the inversed matrix
    setInverse <- function(input) inverse_matrix <<- input 
  
    ## gets the inversed matrix
    getInverse <- function() inverse_matrix  
  
    ## creates a new list that sets the columns to the names of the getter and setter functions   
    list(setMatrix = setMatrix, getMatrix = getMatrix,  
         setInverse = setInverse,
         getInverse = getInverse)
  
}



## returns a cached inverse matrix or calculates and returns an inverse matrix

cacheSolve <- function(x, ...) {
    ## gets inverse matrix
    inverse <- x$getInverse()
  
    ## checks that inverse matrix is not null
    if(!is.null(inverse)) {
    
        ## sends message to user that cached data is being retrieved
        message("getting cached data") 
    
        ## returns value of inverse matrix
        return(inverse)
  }
  
    ##if inverse matrix is null, code below runs  
    ## gets matrix
    data <- x$getMatrix()
  
    ## calculates inverse matrix using matrix as input
    inverse <- solve(data)
  
    ## sets inverse matrix
    x$setInverse(inverse)
  
    ## returns inverse matrix
    inverse
}