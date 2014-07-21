## cachematrix.R
## Programming Assignment 2 for R Programming rprog-005
## Andrew Werth
## July 2014

## Since matrix inversion is a computationally expensive
## operation, these functions provide a wrapper/class that can
## cache a matrix inverse.

## makeCacheMatrix:  
##
## Create an object with methods for storing and retrieving a matrix,
## and for storing and retrieving a cached copy of its inverse             
##
##    x:  Optional matrix to initialize the object with (empty matrix if not provided)
##        We assume that the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
    # inv stores the inverse of the matrix; it's NULL when the cache is empty
    inv <- NULL

    # makeCacheMatrix$set(y):  Sets the matrix held by this object to y and clears cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # makeCacheMatrix$get():  Returns the matrix held by this object, x
    get <- function() x
    
    # makeCacheMatrix$setinverse(inverse):  Sets the cached copy (inv) to the
    #    value 'inverse' (though it doesn't enforce that the copy must actually be
    #    an inverse of x)
    setinverse <- function(inverse) inv <<- inverse
    
    # makeCacheMatrix$getinverse():  Returns the cached inverse matrix, inv
    getinverse <- function() inv
    
    # The constructor returns a list object with all of the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve:
##
## Takes an object created by makeCacheMatrix and returns the inverse of the
## matrix it holds.  If a cached copy is available, it just returns that.
## If there's no cached copy, use 'solve' to find the inverse and store that
## in the cache.
##
##    x:    An object created by makeCacheMatrix
##    ...:  Optional parameters to pass to 'solve' when finding the inverse

cacheSolve <- function(x, ...) {

    # Request the inverse matrix being held by object 'x'
    inv <- x$getinverse()
    
    # If it's not NULL, then we have a cached copy and can return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, get the underlying matrix in x
    data <- x$get()
    
    # And use 'solve' to find the inverse
    inv <- solve(data, ...)
    
    # Store the inverse in the cache and return the inverse matrix
    x$setinverse(inv)
    inv
}
