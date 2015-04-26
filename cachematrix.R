## This code consists of makeCacheMatrix which initializes a list of 4 ##
## functions which are then used by cacheSolve to cache the inverse of ##
## a matrix 									     ##

# You may run the code at command prompt as below :

# > cache_mtx <- makeCacheMatrix(matrix(c(4,2,7,6), nrow = 2))   // Initialize
# > cache_mtx$get()                                  // Check (can be skipped)
# > cacheSolve(cache_mtx)                            // Invert
# > cacheSolve(cache_mtx)                            // Re-call



# makeCacheMatrix: returns a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    # inv will store the cached inverse matrix
    inv <- NULL

    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Get the matrix
    get <- function() x

    # Set the inverse
    setinv <- function(inverse) inv <<- inverse

    # Get the inverse
    getinv <- function() inv

    # Return the functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Computes the inverse of the matrix. If the inverse is already
# calculated before, it skips calculations and returns the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse exists... 
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # Else...
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return the answer
    inv
}