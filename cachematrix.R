## This script contains two functions are used to create a matrix object
## that can cache its inverse as well as a function able to calculate 
## the inverse of a matrix and retrieve the calculation if already cached
### makeCacheMatrix: creates a matrix object that can calculate its inverse
### cacheSolve: computes the inverse of a matrix returned by makeCacheMatrix
### and retrieves a cached inverse if already available

## This function creates a matrix object that can calculate its inverse
## through a list of four functions: set, get, setInverse, getInverse
##
## Example usage: mtrx <- makeCacheMatrix(matrix(1:4,2))
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## This function set the value of the matrix.
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## This function retrieves the value of the matrix.
    get <- function() x
    
    ## This function calculates the inverse of the matrix through the use
    ## of "solve." For more on "solve," see: ?solve.  
    setInverse <- function(solve) m <<- solve
    
    ## This function retrieves the inverse of the matrix as calculated.
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  
}

## This function is used to compute the inverse of the matrix created 
## through the use of the "makeCacheMatrix" function. If the inverse
## is already cached, the function retrieves the cached inverse, skipping
## the computation. In the latter event, the function prefaces its output
## with "Getting cached data...."
##
## Example usage: cacheSolve(mtrx) -- where "mtrx" is the name of the matrix
## created via makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  ## Check for cached data and print the cache if available
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  
  ## If no cached data available, retrieve the value of the matrix, calculate
  ## the inverse, cache the calculation, and print the result.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
