## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### This function creates a special "matrix" object that can cache its
### inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(j) {
    i <<- j
  }
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

### This function computes the inverse of the special "matrix" returned
### by makeCacheMatrix above. If the inverse has already been calculated
### (and the matrix has not changed), then cacheSolve should retrieve
### the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Check if the inverse is cached
  i <- x$getInv()
  if (!is.null(i)) {
    message("Retreiving cached inverse matrix")
    return(i) 
  }
  
  # Generate and store the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  
  # Return a matrix that is the inverse of 'x'
  i
}