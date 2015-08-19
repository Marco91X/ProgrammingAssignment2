## These two functions want to implement a special matrix that cashes its inverse.
## Computing the inverse of a matrix is an extremely expensive operation and re-use a pre-calculated value
## (if the matrix is not changed) can speed up this operation.
## I have implemented two functions:
## - makeCacheMatrix: creates the special matrix
## - cacheSolve: computes the inverse of the matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
        inverse <- NULL
		# Getter and setter methods
		# If the matrix changed reset the inverse
        set <- function(m) {
                matrix <<- m
                inverse <<- NULL
        }
        get <- function() matrix
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## function should retrieve the inverse from the cache

cacheSolve <- function(matrix, ...) {
        inverse <- matrix$getInverse()
		# If the inverse is chached the function returnd it
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
		# Get the "stored" matrix
        data <- matrix$get()
		# Computes the inverse
        inverse <- solve(data, ...)
		# Set the inverse
        matrix$setInverse(inverse)
        inverse
}
