## Two functions are used to create a special object that stores a matrix and caches its inverse

## It creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_i <- function(inverse) i <<- inverse
  get_i <- function() i
  list(set = set, get = get,
       set_i = set_i,
       get_i = get_i)

}

## If it was not calculated before it computes inverse of 'matrix' from makeCacheMatrix
## Otherwise it gets it from cache

cacheSolve <- function(x, ...) {
  i <- x$get_i()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_i(i)
  i
## Return a matrix that is the inverse of 'x'
}
