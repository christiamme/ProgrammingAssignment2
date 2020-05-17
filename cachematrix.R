## makeCacheMatrix: This function creates a special "matrix" object 
##    that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve 
##    retrieves the inverse from the cache.

## Create and cache matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    # the inverse
    set <- function(y) {
      x <<- y    # the matrix
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse or retrieve it from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
