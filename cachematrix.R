## These functions calculate and cache the inverse of a matrix, to save on processor time.
## and to test knowledge of using different environments in R, especially with the <<- assignment operator

## This function handles the caching part

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks to see if the matrix is the same as before and has been inverted already;
## If so, it returns the cached matrix; otherwise, it calculates the inverse and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}



