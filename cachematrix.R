## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function is used to set the matrix and return it through the
## get function, then it set the inverse and return it through the,
## setinverse and getinverse functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse  <- function(inverse) i <<- inverse
  getInverse  <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is used to calculate the inverse of a special matrix
## First it checks whether the inverse is already calculated, if not,
## the function calculate it a return it.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
