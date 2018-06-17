## Catching the mean of a Vector
## for Coursera course

## This function creates a special object that stores a numerix matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setrow <- function() m <<- solve
    getrow <- function() m
    list(set = set, get = get,
         setrow = setrow,
         getrow = getrow)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getrow()    ## Return a matrix that is the inverse of 'x'
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setrow(m)
    m
}
