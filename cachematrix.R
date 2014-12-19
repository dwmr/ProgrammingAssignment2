## These functions work together to improve matrix inversion performance
## by caching an inverted copy of a matrix that can be retrieved many times
## after being constructed once.

## makeCacheMatrix creates a cache object (list) continaing methods to access
## original (non-inverted) and inverted versions of a specified matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(
    set=set,
    get=get,
    set_inverse=set_inverse,
    get_inverse=get_inverse)
}

## cacheSolve asks for the inverted matrix, and if it is not available calculates,
## returns, and stores the inverted matrix in the cache object for subsequent retieval.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
}
