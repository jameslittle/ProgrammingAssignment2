## Solving the inverse of a matrix is an expensive operation
## These two functions provide a caching mechanism whereby
## the inverse is only calculated once.


## Initializes a matrix x. Provides set/get
## functions for x and its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    # set x and inverse in the parent environment
    x <<- y
    inverse <<- NULL
  }
  # getter - returns matrix to caller
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  # return environment to caller
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## calculates the inverse of matrix x (using 'solve')
## if not already set, otherwise returns cached value.
## Note that cacheSolve assumes the matrix is always
## invertible, as per the specification

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # return cached value if not null
  if (!is.null(inv)) { 
    message("using cached inverse")
    return(inv)
  }
  # not cached so get matrix and solve inverse
  M <- x$get()
  # relay additional args to 'solve' via '...'
  inv <- solve(M, ...)
  # cache the inverse matrix
  x$setinverse(inv)
  # return inverse matrix to caller
  inv
}