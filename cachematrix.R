## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
         invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated
## then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invm <- x$getinverse()
    if(!is.null(invm)) {
        message("getting cached data.")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data)
    x$setinverse(invm)
    invm
        
}
