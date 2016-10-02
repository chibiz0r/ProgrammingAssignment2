## This function caches the inverse of a matrix, especially when 
## the matrix doesn't change. This way the inverse doesn't need
## to constantly be recalculated. 

## makeCachedMatrix makes a special "matrix" that 1. Sets a value of
## the matrix 2. Gets the value of the matrix 3. Sets the inverse of
## the matrix 4. Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function()x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special "matrix" and 
## checks if the inverse has already been calculated. If so, it 
## obtains the inverse from the cache. Otherwise, it does the 
## calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
}
