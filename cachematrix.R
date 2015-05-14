## Functions to cache inverses of matrices for quick access

## makeCacheMatrix: Given a matrix, returns  
## a list of functions to (1) set the value of the matrix, 
## (2) get the matrix, (3) set the value of its inverse and 
## (4) get the inverse calculated in (3)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve: given an object created by makeCacheMatrix,
## returns the inverse of the matrix set there (checks if 
## inverse is cached before calculating it).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}