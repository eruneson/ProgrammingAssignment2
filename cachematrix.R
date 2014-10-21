## cachematrix.R contains 2 functions (makeCacheMatrix & cacheSolve) which work
## in unison to solve for the inverse of a matrix 'x', by either computing the
## inverse using the function solve, or retrieving the previously stored inverse
## from cache.

## makeCacheMatrix: creates a list of functions stored as an R object to be 
## utilized by cacheSolve to find/retrieve the inverse of matrix 'x'.
## CALL: objname <- makeCacheMatrix(x)
##  example invertible matrix: x <- matrix(c(-1,0,-5,3,-6,-3,-3,5,1),3,3)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: utilizes the list of functions defined in 'objname' to either
## solve for the inverse of 'x', or returns the solution from cache 
## (the environment) in case it was previously defined.
## CALL: cacheSolve(objname)

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
