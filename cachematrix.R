## makeCacheMatrix creates a special matrix with the following function calls:
## get -- returns the value of the matrix x
## set -- set the value for matrix x
## setinv -- computes the inverse of matrix x by using the 'solve' function
## getinv -- returns the value of the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a function which uses the speical matrix from makeCacheMatrix,
## where the inverse of the special matrix will be looked up in cache, 
## and will compute and store the matrix in cache if it's not yet there.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
