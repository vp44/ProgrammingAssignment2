## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "Matrix" that enables caching the inverse.
## Whenever the Matrix is modified the inverse is reset to Null.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## Initially the inverse is set to NULL
  set <- function(y) {
    x <<- y ## We set the matrix here
    inverse <<- NULL ## since the matrix changed the inverse is set to NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv ## cacheSolve sets the inverse
  getinverse <- function() inverse ## cacheSolve gets teh inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes in the special matrix created by MakeCacheMatrix and returns ...
## the Inverse
## If the inverse is already set and returned then it returns the cachedInverse.
## Otherwise (first time or if matrix was modified) it compute the inverse and ...
## Cache's it before returning.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) { ## If inverse is not null simply return the inverse stored
    message("getting cached data") ## evidence of having a cached inverse
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...) ## compute the inverse since it hasn't been cached
  x$setinverse(inverse) ## cache the inverse before returning
  inverse
        ## Return a matrix that is the inverse of 'x'
}
