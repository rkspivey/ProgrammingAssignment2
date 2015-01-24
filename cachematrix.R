## makeCacheMatrix(x)
## x is a matrix, or will be initialized to an empty matrix by default
## This function creates a list that allows the caller to get and set 
## the value of the matrix, and to get and set the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve(x)
## x should be created by the makeCacheMatrix function
## This function takes an object x, and returns its cached inverse
## matrix if it has already been set.  If the inverse has not been
## computed, this function calculates the inverse, stores the result in
## x, and returns the inverse
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached matrix inverse");
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
