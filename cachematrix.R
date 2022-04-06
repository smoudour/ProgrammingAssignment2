## Put comments here that give an overall description of what your
## functions do

## This function is similar to a constructor class for a matrix. It creates a
## list of functions to 1.set 2.get 3.set the inverse and 4.get the inverse of
## a matrix. When a new matrix is created using function set ($set), it resets
## the inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv
  
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)

}


## This function calculates the inverse of a matrix constructed with the above
## function. If the inverse has been already calculated (or set with the set.inverse)
## function, then it gets the inverse from the cache and does not proceed with
## the calculation

cacheSolve <- function(x, ...) {
  
  inv <- x$get.inverse()
  
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  inv <- solve(x$get(), ...)
  x$set.inverse(inv)
  inv
}
