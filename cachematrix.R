## These functions will check to see if the inverse of the matrix has been solved previously and if it has,
## will return the cached solution.  If not, it will solve the matrix and store the inverse in cache. 

## This function makeCacheMatrix creates a list of functions to 
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the matrix inverse
## Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function solves the inverse of the matrix set by makeCacheMatrix.  
## Before it does this, it checks to see if the matrix has been solved previously.
## If it has, then it returns the cached value.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}