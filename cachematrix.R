## Function to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL    
    set <- function(y = matrix()) {
        # storing the original matrix
        x <<- y
        inv <<- NULL # reset the inverse if the original matrix has changed
    }
    get <- function() x # return the original matrix contained in the object.
    
    # Store and retrieve the inverse matrix.
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by
## the "makeCacheMatrix" above. If the inverse has already been calculated
## (and the matrix has not changed), then it retrieves the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInv() # retrieve the cached inverse from the object
    if(!is.null(inv)) {
        # if the cached inverse exists, return it and exit
        message("getting cached data")        
        return(inv)
    }
    # otherwise, calculate the inverse...
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv) # and cache it in the object.
    inv
}
