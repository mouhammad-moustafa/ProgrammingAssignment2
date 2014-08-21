## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. the following functions enable computation and caching of the inverse of a matrix 


## This function creates a special "matrix" object that can cache its inverse.
## The special matirx has the following functions:
## reset: sets the modified matrix and renitialize cached inverse to NULL.
## 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        reset <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        computeinverse <- function(){
                inverse <<- solve(x)
                inverse
        }
        
        getinverse <- function() inverse
        
        list(reset = reset, getinverse = getinverse,
             computeinverse = computeinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## Then the cachesolve retrieve the inverse from the cache.
## Else the cachesolve compute the inverse of a square matrix the solve function in R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        inverse <- x$computeinverse()
        inverse
}
