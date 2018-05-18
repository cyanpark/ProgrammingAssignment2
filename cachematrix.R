## Caching the Inverse of a Matrix

## set the matrix
## get the matrix
## set the inverse of a matrix
## get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInvMat <- function(invMat) inv <<- invMat
        getInvMat <- function() inv
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## Calcuates the inverse of a matrix
## If the inverse of a matrix has already been calculated,
## gets the inverse of a matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        inv <- x$getInvMat()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInvMat(inv)
        inv
}
