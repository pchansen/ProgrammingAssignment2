## A pair of functions that cache the inverse of a matrix.
##
## The functions follow the style of the example makeVector() and cachemean() code, with
## specific variable name changes and function call changes to match.

## The makeCachMatrix() function creates a list containing functions to:
##      set the value of the matrix - set()
##      get the value of the matrix - get()
##      set the value of the inverse of the matrix - setInverse()
##      get the value of the inverse of the matrix - getInverse()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve() function calculates the inverse of the special cached matrix created 
## with the function above. It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix using the solve() function and sets the value of 
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
