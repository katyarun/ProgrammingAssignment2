## Caching the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object 
## that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    getmatrix <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(getmatrix = getmatrix,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve() computes the inverse of the special "matrix" object
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (& matrix has not changed)
## then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}