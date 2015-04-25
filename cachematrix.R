## General description of functions in this file:
## cacheSolve returns the solution (inverted matrix) for a provided matrix of 
## our special type created by makeCacheMatrix

## A function that creates our special matrix.
## This matrix is actually a list that holds functions for 
## accessing and setting the value of the matrix and its inverse.
### set - stores the matrix value and sets the solution to NULL;
### get - retrieves the input matrix;
### setSolved - stores the provided solution;
### retrieves the stored solution.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL # reset the solution
        }
        
        get <- function() x
        
        setSolved <- function(solved) m <<- solved
        
        getSolved <- function() m
        
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)
}


## This function returns a matrix that is the inverse of given matrix.
## It will retrieve a cached solution (inverse of a matrix in our case)
## or - if it does not yet exist; calculate it and store it for later.
## The function takes our special CacheMatrix (created by the function above)
## as an argument and tries to retrieve a solution that is stored for it.
## If there is a solution currently stored it is retrieved and returned.
## If there is no solution currently stored (it's a NULL value) we retrive
## the stored matrix, calculate its invert and store the solution for future 
## use before returning it.

cacheSolve <- function(x, ...) {
        m <- x$getSolved()
        
        if(!is.null(m)) {
                message("Getting a previously cached solution")
                return(m)
        }
        # if there is no previous solution cached 
        data <- x$get()
        m <- solve(data, ...)
        x$setSolved(m)
        m
}
