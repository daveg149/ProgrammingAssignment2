## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  
  # Does the inverse and sends it to setinverse ("cache")
  setinverse <- function(solve) inversem <<- solve 
  
  # Function to retrieve the inverse
  getinverse <- function() inversem
  
  # The output is a list of 4 elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheInverse <- function(x, ...) {
    inversem <- x$getinverse()
    # If it exists already, it will return a message and end the function
    if(!is.null(inversem)) {
      message("getting cached data")
      return(inversem)
    }
    
    #if it doesn´t exist, the function calculates and returns the inverse
    data <- x$get()
    inversem <- solve(data, ...)
    x$setinverse(inversem)
    return(inversem)
  }
  
  
}
