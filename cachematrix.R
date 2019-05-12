## Functions Creates matrices and cahces as necesarry

## Caches a matrix in an object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## Takes an inverse of a matrix, and caches it.
## If it already executed, it retrieves matrix from cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
            message("Getting the cached matrix")
            return(m)
    }
    data <- x$get()
    m <- solve(data)
    message("Setting the cached matrix")
    x$setmatrix(m)
    return(m)
}

##Sample output
##Setting the cached matrix
##Getting the cached matrix
##Getting the cached matrix
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
x <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
y <- cacheSolve(x)
y <- cacheSolve(x)
y <- cacheSolve(x)
y