##makeCacheMatrix creates a list of functions to set the matrix and cache the inverse of the matrix
##cacheSolve computes the inverse of matrix if it is not cached yet and matrix is not changed and caches the inverse


##inv stores inverse of the matrix, set <- sets the matrix, setinv <- sets the inverse of matrix
##get and getinv gets the value of matrix and its inverse respectively
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y){
         x <<- y
         inv <<- NULL
        }
        get <- function(){ x }
        setinv <- function(inverse){ inv <<- inverse}
        getinv <- function(){ inv }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##Return a matrix that is the inverse of 'x' either cached or computed
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
           message("getting cached data")
           return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}


