## makeCacheMatrix takes an invertible matrix and exposes four methods for interacting with it:
## get() - retrieves the matrix
## set(x) - replaces the matrix 
## setInverse(x) - stores an inverse of the matrix
## getInverse() - retrieves the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve takes a cacheMatrix "object" created with the makeCacheMatrix function and returns the
## inverse of the matrix, attempting to retrieve a cached copy of the inverse before computing it.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(is.null(inverse)) {
    inverse <- solve(x$get())
    x$setInverse(inverse)
  }

    inverse
}
