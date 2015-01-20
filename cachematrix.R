## These functions provide functions for caching the inverse of a
##  matrix to prevent it having to be recalculated multiple times.
##  Use the makeCacheMatrix function to construct the cached matrix 
##  supplying the matrix to be solved as an argument. Use the cacheSolve
##  function to calculate the inverse of the matrix, supplying the 
##  special cached matrix created by the makeCacheMatrix function.

## This function creates a special "cache matrix" using the 
##  supplied matrix.  The inverse of the matrix can be 
##  acquired using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a special "cache matrix" created with the 
##  makeCacheMatrix function and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
