## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL ## set inv to NULL, will hold value of inverse
    set <- function(y){ ## set used to set the value of matrix in environment
        x <<- y 
        inv <<- NULL ## if new matrix, inv is set to NULL once again
    }
    get <- function() {x} ## returns the value of matrix
    setinverse <- function(inverse) {inv <<- inverse} ## sets the value of inv
    getinverse <- function() {inv} ## returns the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
## create list for functions to subset
}


cacheSolve <- function(x, ...) {
        inv <- x$getinverse
        if(!is.null(inv)){ ## function for checking if inverse is already taken
            message("getting cached data")
            return(inv)
        } ## if inverse is not yet taken, then solve for inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
