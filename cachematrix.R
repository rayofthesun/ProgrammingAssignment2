## makeCacheMatrix and cacheSolve functions are written to 
## handle the time overhead and to avoid repeated computations
## It saves the time by storing in cache of already computed inverse 
## of a matrix-which can be re-used for the same matrix - instead 
## of recomputing


##function makeCacheMatrix   

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)){
        message("getting cached data")
    }
    inverseData <- x$get()
    inverse <- solve(inverseData, ...)
    x$setsolve(inverse)
    inverse

}
