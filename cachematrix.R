## These functions cache the inverse of a matrix. 
## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse. The object returned by this function is 
## later used by cacheSolve that computes the inverse of the special "matrix".

## This function will create the special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mx <<- inverse
  getinverse <- function() mx
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Will compute the inverse of the special "matrix"
## Need to load the package "matlib"
library(matlib)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mx <- x$getinverse()
  if (!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- inv(data, ...) # you can also use solve() 
  x$setinverse(mx)
  mx
}
