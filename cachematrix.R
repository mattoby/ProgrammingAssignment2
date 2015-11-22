## This source file includes a set of two functions, one of which holds a matrix
## (and the matrix's inverse), and the other of which determines the inverse of
## the matrix when passed the output of the first function (which is also a
## function). For coursera R programming, assignment 2
#
## Matt Oberhardt, 11/21/2015


## this makes a function, which holds information about a matrix. The matrix's
## inverse, once calculated, is cached in the outer function, i.e., the
## environment of the inner function (via lexical scoping). The contents of the
## matrix can be set as such:
# usage:
# CM <- makeCacheMatrix()
# CM$get() # outputs matrix
# CM$set(<<define matrix to be set here>>)
# CM$getinv() # outputs inverse of the matrix
# CM$setinv() # this can be used to set the inverse..

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a CM object (produced by makeCacheMatrix) and sets the
## inverse of the matrix.
# usage:
# inv <- cacheSolve(CM) # inv is the inverse of the matrix
# all.equal(CM$getinv(),inv) # this shows that the inverse was set in CM

cacheSolve <- function(x, ...) {
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
