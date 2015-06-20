## This function stores four functions in a sub-environment
##      One retrieves the input matrix from an outer-environment
##      Two determines the inverse of the input matrix
##      Three retrieves the inverse matrix that has been cached
##              The inverse may be null if none has been cached yet
##      Four creates a list of the four functions stored by this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function takes the input matrix vector from the makeCacheMatrix function
## and caches it's inverse in a sub-environment vector
##
## Before it performs the function, it tests to see if the inverse matrix is already cached 
## in the sub-environment
##
##      If the inverse matrix exists in the sub-environment, the function returns 
##      the inverse matrix already cached. This potentially saves a lot of system resources.
##
##      If the inverse matrix is not found, the function creates the inverse matrix 
##      and caches as a vector in the sub-environement

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
