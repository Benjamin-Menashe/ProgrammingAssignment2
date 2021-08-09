## This pair of functions create a special matrix and caches the inverse of that matrix

## The first function creates a special "matrix", which is really a list containing functions to set & get the matrix
## and set & get the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function calculates the inverse of the special "matrix".
## It first checks to see if the mean has already been calculated, and if so, gets it from the cache.
## Otherwise, it calculates the inverse and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
