## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# $set          set matrix value
# $get          get matrix value, in particular to be called by cacheSolve()
# $setinv       set the inverse of the stored matrix value, in particular to be called by cacheSolve()
# $getinv       get the inverse of the stored matrix value  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
# If there is a cached inverse, return cached inverse.
# If there is no cached inverse, compute matrix inverse using solve(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    new_matrix <- x$get()
    inv <- solve(new_matrix)
    x$setinv(inv)
    inv        
}









