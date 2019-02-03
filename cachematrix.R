## Put comments here that give an overall description of what your
## functions do

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize i
  i <- NULL
  
  # Define set, which changes the matrix and invalidates the cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Define get, which returns the matrix
  get <- function() x
  
  # Cache a calculated inverse, and get the cached value
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  # Return control list containing above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Gets the inverse, if it exists
  i <- x$getInverse()
  
  # If the matrix X contains a cached value, return that & do not calculate
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If uncached, get the inverse of the matrix and set it to cache var i
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
