## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix it's a special vector that stores 4 functions
## It works like an Class with methods and variables
## The 4 functions are:
## set: uses it for setting your matrix. The matrix can also be set when you create the object
## get: uses it for getting your matrix
## setivnerse: uses it for setting the inverse of your matrix
## getivnerse: uses it for getting the inverse of your matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that optimize the calculation of a Matrix's inverse
## before it calculates the inverse it checks if the inverse is already calculated
## If it's already calculated it gets the stored value
## Otherwise the function calculates the inverse and store the result for future utilizations

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
