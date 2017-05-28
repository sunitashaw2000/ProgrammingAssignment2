## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## The following functions makeCacheMatrix & cacheSolve are going to do the same.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This object is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Output
## Program is sourced as 
## source('CacheMatrixInverse.R')
## Create a square invertible matrix as follows 
## > x <- matrix ((1:4), nrow=2, ncol=2)
## > m <- makeCacheMatrix(x)
## First run for get()
## > m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## First Run for Inverse of Matrix returns NULL
## > m$getinverse()
## NULL
## First Call of cache creates the inverse
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Second run gets from cache
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
