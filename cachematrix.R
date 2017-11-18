## The functions below allow us to calculate the inverse of a matrix one time
## and cache the inverse rather than repeatedly calculating it.


## This function creates a special matrix that caches the inverse
## It includes setter and getter methods for the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse if it does not exist, and
## returns the cached value otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
