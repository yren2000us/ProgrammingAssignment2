## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invs <<- inverse
    getInverse <- function() invs
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getInverse()
    if (!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    mat <- x$get()
    invs <- solve(mat, ...)
    x$setInverse(invs)
    invs
}