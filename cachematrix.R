## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # Set the matrix
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    
    # Get the matrix
    get <- function() x
  
    # Set the inverse of the matrix
    setInverse <- function(inverse){
      i <<- inverse
    }
    
    # Get the inverse of the matrix
    getInverse <- function() i
    
    # Make the list and 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      # Retrieve the inverse
      i <- x$getInverse
      
      # Check whether the inverse is cached or not, Return the inverse if it is cached
      if(!is.null(i)){
        message("getting cached data")
        return(i)
      }
      
      # Get the matrix
      matrix <- x$get()
      
      # Calculate the inverse of the matrix
      # Used %*% for matrix multiplication
      i <- solve(matrix) %*% matrix
      
      # Set the inverse of the matrix
      x$setMean(i)
      
      #return the matrix
      i
}
