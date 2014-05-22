## The function makeCacheMatrix creates a special list of three functions which,
## 1. get the value of matrix "x".
## 2. set the value of the inverse of matrix "m"
## 3. get the value of the inverse of matrix "m"

## The function receives as input argument a square matrix "x" and returns a list of three functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        getmatrix <- function() x                          ## 1. get the value of matrix "x".
        setinverse <- function(inverse) inv <<- inverse    ## 2. set the value of the inverse of matrix "m"
        getinverse <- function() inv                       ## 3. get the value of the inverse of matrix "m"
        list(getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse) ## a list of functions is returned
}

## The function cacheSolve performs the following functions,
## 1. gets as input argument a list "x" containing three functions 
## 2. checks whether the inverse of matrix "x" was already calculated. If not, the inverse of "x" is calculated, stored
##    in the cache and returned, otherwise, it is just retrieved from the cache without performing any computation.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()             ##checks whether the inverse of matrix "x" was already caculated.
        
        if(!is.null(inverse)) {
             message("getting cached data")
             return(inverse)                  ## If already calculated, it is just retrieved from the cache.
         }
       
        data <- x$getmatrix()                 ## If not calculated,
        inverse <- solve(data, ...)           ## the inverse is calculated,
        x$setinverse(inverse)                 ## stored in the cache,
        inverse                               ## and returned by the function
}
