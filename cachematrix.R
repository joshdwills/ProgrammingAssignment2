## Put comments here that give an overall description of what your
## functions do

## This function sets a matrix, gets the matrix, sets the inverse
## of the matrix, then gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         set = setinverse,
         getinverse = getinverse)
}


## This function checks if the inverse for a matrix is already calculated
## and cached. If so, it gets the inverse from the cache and skips the
## computations. If not, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- inverse(data, ...)
    x$setinverse(inv)
    inv
}
