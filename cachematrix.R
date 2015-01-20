## Functions in this file are used to efficiently compute inversion of a given matrix. 
## If inversion have already been calculated, return cached value

## Create list of functions giving access to original matrix and to it's inverted version cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Perforcm check if inversion of matrix x has been already computed and if so return inversion. 
## If not, compute inversion, store it in cache and return

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
