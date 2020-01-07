## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    x_inv <- NULL # initialize inverse of x to be NULL
    
    set <- function(y) { # set value of matrix x to be y 
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x # get value of matrix x 
    setinv <- function(matrix_inv) x_inv <<- matrix_inv # set inverse of matrix x
    getinv <- function() x_inv # get inverse of matrix x
    
    list(set = set, # return list of objects 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
    
}


## The function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    x_inv <- x$getinv() # get inverse of matrix 
    
    if (!is.null(x_inv)) { # if inverse has already been calculated take this inverse 
        message("getting cached data")
        return(x_inv)
    }
    
    data <- x$get() # if inverse has NOT been calculated yet get matrix for which inverse is to be calculated 
    x_inv <- solve(data,...) # calculate inverse
    x$setinv(x_inv) # set inverse
    x_inv # return inverse
    
}
