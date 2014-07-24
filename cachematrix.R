## The two functions calculate and cache the inverse of
## a matrix. When a user want to get the inverse, it first
## checks whether the inverse exists, if exists then use
## that value directly to avoid duplicate calculation, otherwise
## calculate the inverse, and cache the value.

## makeCacheMatrix is used to set and get a matrix.
## at same time, it can set and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve get the inverse of a matrix.
## If the inverse is cached already, then use that value.
## Otherwise compute the inverse and cache it.

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
