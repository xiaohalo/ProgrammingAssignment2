## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "Matrix", which is really 
## a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function()  inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve calculates the inverse of the special "matrix" created 
## with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the inverse of 
## the matrix in the cache via the setInv function.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
