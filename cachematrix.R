## These functions are caching inverse of a matrix. 

## This function creates "matrix" object that can cache itself. 
## It returns list of functions containing function for 
## set and get matrix 
## set and get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
  
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
