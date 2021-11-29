## Put comments here that give an overall description of what your
## functions do

## Making the function which creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Now making a function that either computes the inverse of the special 
## "matrix" or retrieves the inverse from the cache in case the inverse has 
## already been calculated

cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
         if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         mtr <- x$get()
         inv <- solve(mtr, ...)
         x$setInverse(inv)
         inv
}
