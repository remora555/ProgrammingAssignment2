## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## A list is made with 4 functions setting and getting the matrix and
## setting and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        setM <-function(y) {
                x <<-y
                inv <<- NULL
        }
        getM <- function() x
        setinv <- function(solve) inv<<-solve
        getinv <- function() inv
        list(setM = setM, getM = getM,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getM()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}  

