## This function creates a special "matrix" object that can cache its inverse.
## Example :
## x <- c(1, 2, 3, 0, 1, 4, 5, 6, 0)
## dim(x) <- c(3,3)
## my_matrix$set(x)
## my_matrix$setInverse()
## my_matrix$getInverse()
## should print :
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setInverse <- function()
    inv <<- solve(x) 
  getInverse <- function()
    inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
