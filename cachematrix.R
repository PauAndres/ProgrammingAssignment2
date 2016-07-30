## We create two function to obtain the inverse of a matrix froma the cache if this
## inverse has been calculated before, in order to minimize time consuming.

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
              set <- function(y) {
                        x <<- y
                        inv <<- NULL
              }
            get <- function() x
            setInverse <- function(inverse) inv <<- inverse
            getInverse <- function() inv
            list(set = set,
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}

## This function computes the inverse of the "matrix" created before in 
## makeCacheMatrix. If the inverse has been calculated before, then
## is obtained from the cache.

cacheSolve <- function(x, ...) {
           ## Return a matrix that is the inverse of 'x'
           inv <- x$getInverse()
            if (!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
            }
          mat <- x$get()
          inv <- solve(mat, ...)
          x$setInverse(inv)
          inv
}

