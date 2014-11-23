##  Functions to compute the inverse of a matrix, and save the inverse in a cache.
 
# makeCacheMatrix creates a special "matrix", which is really a list containing a 
# function to
#
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Computes the inverse of a matrix, and upon repeated calls
## returns the inverse if the matrix has not change. 
## Purpose:  Computing the inverse can be expensive in terms
## of computer time.  Caching the result can improve performance
## by reducing redundant calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

