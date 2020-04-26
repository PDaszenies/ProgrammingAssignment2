## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y){
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inversa <<- solveMatrix
        getInverse <- function() inversa
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInverse()
        if(!is.null(inversa)){
                message("getting cached data")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data)
        x$setInverse(inversa)
        inversa
}
