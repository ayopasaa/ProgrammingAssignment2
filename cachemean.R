## makeCacheMatrix creates an special vector, which is a list containing a function to set value, get value, set value, 
##get value of solve(inverse of a matrix)

## matrix is the type of data

makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        ##set matrix
        set <- function(y) {
                m <<- y
                inverse <<- NULL
        }
        ##get() is the return of the matrix
        get <- function() m
        ## sets (does not compute) the value of the inverse of the matrix m
        setinverse <- function(inv) inverse <<- inv
        ## return the cached value of the inverse of the matrix.
        ## NULL if the inverse has not yet be set, or the underlying
        ## matrix has changed since the last invocation
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and
##skips the computation. Otherwise, it calculates the inverse of the matrix data and sets the value of the mean in the cache 
##via the setsolve function.

cacheSolve <- function(m, ...) {
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}
