## This module contains a pair of functions that allow the inverse
## of a matrix to be computed, cached, and retrieved.

## Given a matrix x, return a list L of four functions:
##   L$set: Set the matrix associated with L.
##   L$get: Get the matrix associated with L. This is
##     initially set to the matrix x.
##   L$setinverse: Set the inverse of the matrix associated
##     with L.
##   L$getinverse: Get the value last set with L$setinverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a list L produced by makeCacheMatrix:
##   1. Return the cached value of the matrix x associated
##      with L if an inverse value has been cached.
##   2. Otherwise, compute the inverse via solve(x, ...)
##      where ... are any additional arguments passed to cacheSolve.
##      The computed inverse is set on L's cached and returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
