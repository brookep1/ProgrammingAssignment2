#######
## -- Caching the Inverse of a Square Matix --
## Solving the inverse of a large matrix can be slow
## These functions provide a cache of the solution

#######
# Functions:
## makeCacheMatrix
## CacheSolve

########
# Limitations:
## This code will only work on square matrices
## Not all matrices are solveable -- will get an "unable to find stored_matrix"
## error if it can't solve the matrix
#
# Credits:
## These functions were created for "Assignment 2" of the
## Coursera "R Programming" course

## makeCacheMatrix creates a matrix (function) that can cache it's inverse
## Note it does not perform the inverse itself
## it exposes the following methods for manipulating the matrix and it's inverse
# set   - Sets the matrix
# get   - Gets the matrix
# getinverse - Gets the currently cached inverse
# setinverse - Sets the inverse

makeCacheMatrix <- function(stored_matrix = matrix()) {
    # initialize stored_inverse to a NULL
    stored_inverse <- NULL
    # Store the matrix persistently
    set <- function(matrix) {
        stored_matrix <<- matrix
        stored_inverse <<- NULL
    }
    # Retrieve the "stored" matrix
    get <- function() stored_matrix
    # Store the computed inverse
    setinverse <- function(inverse) stored_inverse <<- inverse
    # Retrieve the stored inverse
    getinverse <- function() stored_inverse
    # The "methods" are actually the indices of a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of a square matrix and caches it with the
## setinverse method
## once cached getinverse will retrieve the solution

cacheSolve <- function(cache, ...) {
        inverse <- cache$getinverse()
        # If cache is populated use that
        if(!is.null(inverse)) {
            message("Getting Inverse from Cache")
            return(inverse)
        } 
        # Retrieve the matrix, solve (invert) it, and return the inverse
        mx <- cache$get()
        # tryCatch will show any error for solve and prevent an R script error 
        ix <- tryCatch( solve(mx) )
        cache$setinverse(ix)
}

#########
# Example
#########
## Need a solveable matrix. Had to google one.
###http://www.purplemath.com/modules/mtrxinvr2.htm
##
# mat <- matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3)
# mat.cache <- makeCacheMatrix(mat)
# mat.cache$get()
# cacheSolve(mat.cache)