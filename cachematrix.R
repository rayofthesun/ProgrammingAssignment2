## makeCacheMatrix and cacheSolve functions are written to 
## handle the time overhead and to avoid repeated computations
## It saves the time by storing in cache of already computed inverse 
## of a matrix-which can be re-used for the same matrix - instead 
## of recomputing


##function makeCacheMatrix creates a special vectori, containing 
## a function to set/get the matrix and its inverse

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


## cacheSolve calculates the inverse of the special matrix
## created in the makeCacheMatrix function. 
## It first checks if inverse in present in the cache, if not
## it calculates the inverse and stores it in the cache

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
