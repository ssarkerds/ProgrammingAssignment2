## Matrix inversion is usually a costly computation. 
## The following functions cache the inverse of a matrix rather than compute it repeatedly
## Note: This solution makes an assumption that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse
## The get and set functions 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # returns the matrix 'x'
  get <- function() x
  
  # sets the given matrix to 'x' and clear cache. 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # returns the inverse of the matrix x
  getinv <- function() m
  
  # caches the inverse of the matrix 'x'.
  setinv <- function(inv) m <<- inv 
  
  # return a list containing a function to get and set x and its inverse. 
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # if the inverse of 'x' is in cache, return it.
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # inverse of 'x' is not available in cache, so compute it.
  data <- x$get()
  m <- solve(data)
  
  # store the inverse in cache.
  x$setinv(m)
  
  # return the inverse of 'x'
  return(m)
}
