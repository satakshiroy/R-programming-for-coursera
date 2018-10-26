## The given code caches the Inverse of a Matrix:
## The makeCacheMatrix function creates a special "matrix" object that  caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" created by makeCache function following specific conditions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ##if inverse is already calculated,this function retrives the inverse from the cache
        if (!is.null(inv)) {
                message("retrieve the cached inverse matrix")
                return(inv)
        }
        ##if the matrix is a square invertible one, inverse is calculated by using "solve" function in R
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}