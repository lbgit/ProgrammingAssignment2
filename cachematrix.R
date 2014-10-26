## Assignment 2: Caching the Inverse of a Matrix
## L. Brown. For R Programming, Coursera.org

## This function creates a special matrix object that can cache its inverse
## It creates a list of functions to set the value of the matrix, get the
## value of the matrix, calculate the inverse of the matrix, and get the 
## inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    # return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special matrix object, returned by
## makeCacheMatrix. If the inverse has been already calculated (and the matrix
## has not changed) then this function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    # return a matrix that is the inverse of 'x'
    inv
}