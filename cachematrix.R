## The first function, makeCacheMatrix creates a special "matrix" 
## object that can cache the input matrix and its inverse
##
## setMatrix is a function that set the value of the matrix
## getMatrix is a function get the value of the matrix
## setInverse is a function that set the value of inverse of the matrix
## getInverse is a function that get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## The following function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, and sets the value in the cache via setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
