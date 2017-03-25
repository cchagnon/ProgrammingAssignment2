## JHU Science Specialization, R Programming Week 3
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix
## Created by: cchagnon
## Created on: 2016-03-25
## 
## Summary
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## (there are also alternatives to matrix inversion that we will not discuss here)
## Our assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ##
    ## Function 1: makeCacheMatrix()
    ## This function creates a special "matrix" object that can cache its inverse.
    ##
    
    mInverse <- NULL
    # 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    
    # 2. get the value of the matrix
    get <- function() x
    
    # 3. set the value of the inverse
    setMatrixInverse <- function(inverse) mInverse <<- inverse
    
    # 4. get the value of the inverse
    getMatrixInverse <- function() mInverse
    return(list(set = set, get = get,
                setMatrixInverse = setMatrixInverse,
                getMatrixInverse = getMatrixInverse))

    }

cacheSolve <- function(x, ...) {
    ## 
    ## Function 2. cacheSolve()
    ## The following function calculates the inverse of the special "matrix" created
    ## with the above function. However, it first checks to see if the inverse has
    ## already been calculated. If so, it gets the inverse from the cache and skips
    ## the computation. Otherwise, it calculates the inverse of the data and sets
    ## the value of the inverse in the cache, via the setMatrixInverse() function.
    ##
    
    mInverse <- x$getMatrixInverse()
    ## 1. see if cached matrix exists
    if (!is.null(mInverse)) {
        message("getting cached data")
        ## exists: retrieve the inverse from the cache and return
        return(mInverse)
    }
    
    ## 2. calculate inverse
    data <- x$get()
    mInverse <- solve(data, ...)
    ## cache calculated inverse
    x$setMatrixInverse(mInverse)
    ## return calculated inverse
    return(mInverse)
}

