## These functions enable calculation of the inverse of a matrix
## The calculation is cached to minimize time used for repeated calculations

## makeCacheMatrix
## Creates a list that can cache the calculation of the inverse of a matrix
## Return a list that consists of:
## 1. a set function to save the matrix
## 2. a get function to get the matrix
## 3. a function to set the inverse of the matrix
## 4. a function to get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## Calculates the inverse of a matrix and caches the result
## repeated calls using the same matrix x will return the cached result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
