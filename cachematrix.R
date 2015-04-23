## This R file contains two functions, makeCacheMatrix and cacheSolve
## These two functions are used in conjunction to cache and return the inverse of a matrix

## makeCacheMatrix takes a matrix and returns a list of functions to to 
## set the matrix,
## get the matrix,
## set the matrix inverse, and
## get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve takes the list of functions returned by makeCacheMatrix and 
## calculates the inverse of the matrix set in makeCacheMatrix if it is not yet cached
## retrieves the inverse of the matrix set in makeCacheMatrix if it is already cached

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

