## makeCacheMatrix() and cacheSolve() allows to create a matrix and calculate and store 
## it's inverse in cache, so if it's needed to get to the console again there is no need 
## for R to compute all over again

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix, if it's already computed it retrieves the value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
