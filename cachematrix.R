## Script to calculate the inverse of a SQUARE matrix. First a special object 
## that stores the matrix is created via `makeCacheMatrix`.  Then `cacheSolve` 
## returns the inverse of the matrix (directly computed via solve(X) or from 
## the cache)

print ("ATTENTION! THE MATRIX MUST BE SQUARE!") #message for the user

## MAKECACHEMATRIX(X = MATRIX())
#   The function creates a special "matrix" object that can cache its inverse.
#   Is a list containing a function to
#       1.  set the value of the matrix: set(matrix)
#       2.  get the value of the matrix: get()
#       3.  set the value of the inverse: setinv(inverse)
#       4.  get the value of the inverse: getinv() 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinverse = setinv,
         getinverse = getinv)
}


## CACHESOLVE (X, ...)
# This function computes the inverse of the special "matrix" returned by
# `makeCacheMatrix`. If the inverse has already been calculated (and the 
# matrix has not changed -in that case, the returned value would be NULL), 
# then `cacheSolve` retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
