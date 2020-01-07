## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(matrix_inv) x_inv <<- matrix_inv 
    getinv <- function() x_inv 
    
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## The function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if (!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data,...)
    x$setinv(x_inv)
    x_inv
    
}
