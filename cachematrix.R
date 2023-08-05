## Functions for caching and retrieving inverse of a square matrix
## Usage: cacheSolve(makeCacheMatrix(yourmatrixname))

## makeCacheMatrix returns a list of handy functions to get, set the matrix and
## to get and set the inverse of the matrix with getCmatrix and setCmatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCmatrix <- function(solve) m <<- solve
        getCmatrix <- function() m
        list(set = set, get = get,
             setCmatrix = setCmatrix,
             getCmatrix = getCmatrix)
}


## cacheSolve makes use of the makeCacheData to check if the inverse of 
## the matrix is already in cache and skips the calculation in that case. 
## otherwise it calculates it and stores it in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getCmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCmatrix(m)
        m
}
