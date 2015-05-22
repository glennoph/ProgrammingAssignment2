## provide a cache of the matrix and the solution of a system of equations
## makeCacheMatrix holds the matrix x and the solution, inv
## cacheSolve will either return the cached solution or will solve and cache the inverse

## make cache of matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
    # init inv matrix to null meaning not set
    inv = NULL 
    # set is a function that sets matrix x and nulls the inverse (inv)
    # note that x and inv reside in the parent 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get is a function that returns matrix x
    get <- function() { x }
    # setinv with a parameter, sets the inv in the parent
    setinv <- function( solve ) { inv <<- solve }
    # setinv with no parameters, returns inv
    setinv <- function() { inv }
    # list ...
    list( set=set, get=get, setinv=setinv, getinv=getinv )
}

## cacheSolve is a fuction that returns either the cached inverse (inv) 
## or solves the matrix, caches the inverse, and returns the inverse

cacheSolve <- function(x, ...) {
    # inv is a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # if the cached inverse (inv) is not null then write a message and return inv
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if code gets here then the inverse is solved and cached for later
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
