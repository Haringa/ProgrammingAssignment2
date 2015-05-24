## Function makeCacheMatrix converts a regular R matrix to a
## data structure that is able to store the matrix as well as
## its inverse. Function cacheSolve interacts with this data
## structure by computing and storing the inverse if it isn't
## stored yet; if it has been stored already, the function
## will just retrieve it in an attempt at efficiency.

## Create matrix + inverse data structure.
makeCacheMatrix <- function(x = matrix()) {
    # No inverse at initialization.
    inverse <- NULL
    # If matrix is changed by set function, remove inverse.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(value) inverse <<- value
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return inverse, computing it only the first time it is called.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    # If the inverse had been set before, simply return that.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # If inverse had not been set, compute set and return it.
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
