## These functions illustrate how to create a special matrix, which can cache
## it's inverse (using the solve() function)

## Creates a new special matrix object, with a cachable 'inverse' calculation, based on
## an input matrix object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Takes a special 'makeCacheMatrix' matrix object as input and returns it's inverse. If
## the inverse matrix already exists (in cache), the inverse is returned -- otherwise 
## it does new calculation of the inverse of the input 'makeCacheMatrix' matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
