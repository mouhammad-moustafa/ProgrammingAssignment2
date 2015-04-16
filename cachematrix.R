## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. the following functions enable computation and caching of the inverse of a matrix 


## This function creates a special "matrix" object that can cache its inverse.
## The special matirx is really a list containing a function to:
## set: set the value of the matrix and renitialize cached inverse to NULL.
## get: get the value of the matrix
## setinverse: set the value of the inverse
## getinverse: get the cached inverse if already computed NULL otherwise

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse){
                inv <<- inverse
        }
        
        getinverse <- function() inv
        
        list(set = set, get = get, getinverse = getinverse,
             setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## Then the cachesolve retrieve the inverse from the cache.
## Else the cachesolve compute the inverse of a square matrix using solve method 
## and caches the computed value in a variable.
## We assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
