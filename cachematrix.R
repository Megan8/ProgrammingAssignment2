## The functions below cache the value of a matrix's inverse so that
## it can be looked up in the cache rather than being recomputed. The
## functions assume that the matrix supplied is always invertible.

## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse
## It really returns a list of functions to 1) set the value of the 
## matrix, 2) get the value of the matrix, 3) set the value of the 
## inverse, 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## inv will store the cached inverse matrix
  inv <- NULL
  
  ## define the four functions that go in the list
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) inv <<- solve

  getinv <- function() inv
  
  ## returns the list of the functions defined above
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the
## matrix has not changed, then cacheSolve retrieves the inverse from
## the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  ## if inverse has already been calculated, return it with a message
  ## saying that it is from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if inverse has not been calculated already, calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  ## cache the inverse so it can be used again in the future
  x$setinv(inv)
  
  ## return inverse
  inv
}
