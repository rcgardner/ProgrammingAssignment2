## cachematrix.R caches potentially time consuming operations

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of x returned by 
## makeCacheMatrix.  If the inverse has already been calculated and
## the matrix has not changed, then cacheSolve retrieves the inverse
## from the cache

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
