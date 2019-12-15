## Put comments here that give an overall description of what your
## functions do


## The first function, makeCacheMatrix, embeds a list of values that
## will be stored as variables within x (e.g. x$getmat) in a certain envi-
## ronment. The second function reads makeCacheMatrix and first sees
## whether the inverse to the matrix x has already been solved and stored
## within that environment. If so, it prints that result. If not, it
## calculates the inverse itself and stores it within makeCacheMatrix as
## x$getinv.


## Write a short comment describing this function


## This function stores a list of values related to matrix "x" and cal-
## culating its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        setmat <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(solve) mat_inv <<- solve
        getinv <- function() mat_inv
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function


## This function uses makeCacheMatrix to find the inverse of "x," either by
## retrieving the cached value of solve(x) via the x$getinv variable or by
## retrieving the value of the matrix "x" stored in the x$getmat variable
## and using x$setinv to calculate its inverse. If it does the latter, it
## then caches the inverse to x$getinv.

cacheSolve <- function(x, ...) {
        inversion <- x$getinv
        if (!is.null(inversion)) {
                message("getting cached data")
                return(inversion)
        }
        get_mat <- x$getmat
        inversion <- solve(get_mat, ...)
        x$setmat
        return(inversion)
}