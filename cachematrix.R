## Put comments here that give an overall description of what your
## functions do

## the function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function (){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

  



## the function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  term <- x$get()
  inv <- solve(term, ...)
  x$setInverse (inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
