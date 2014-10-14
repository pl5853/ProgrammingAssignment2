## These functions are designed to cache the inverse-calculation of a matrix. 
## As long as the matrix doesn't change, the calculation is done only once

## The function makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve returns the inverse of the special "matrix". The 
## cached data is returned as long as it's still valid for the specified matrix,
## otherwise the cache is populated with the correct value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
