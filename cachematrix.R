## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  return (list(get = get, set = set, getSolve = getSolve, setSolve = setSolve))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}
