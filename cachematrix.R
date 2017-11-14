## Set of functions that allow a user to create an object that can cache the inverse of a Matrix, 
## reducing compuational times if the same calculation has to be repeated several times
## 
## -makeCacheMatrix will create the object and the set of operations allowed in that object
## -cacheSolve will return the inverse of the matrix, either the cached value or the result of a new
## calculation if no cached value, for that Matrix, is available


## makeCacheMatrix:
## In this function, the object is created, along with a set of operations allowed over that object:
## - set: will assign a new matrix to the object. This will also erase previous cached inverse calculation
## - get: will return the currently saved matrix
## - setInverse: will cache the indicated inverse
## - getInverse: will return cached inverse
##
## Please note that no calculation is performed in this function: just getting and setting attributes


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


## cacheSolve
##
## In this function, the actual calculation occurs if no previous calculation for the same Matrix is 
## already cached
##
## Function starts by verifying if a previous result is available. If it is, cached data will be returned.
## Otherwise, a new calculation is executed and the result is once again cached, using setInverse function from 
## makeCacheMatrix


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) ## Return a cached matrix that is the inverse of 'x'
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv         ## Return a matrix that is the inverse of 'x'
           
}
