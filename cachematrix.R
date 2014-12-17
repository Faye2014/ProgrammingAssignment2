## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function makeCacheMatrix creates a special "Matrix" object that can cache its inverse
# 'makeCacheMatrix' function creates a special "Matrxi", which is really a list containing a functions to

#   1. set the value of the matrix
#   2. get the value of the vector
#   3. set the value of the inverse
#   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

# The function 'cacheSolve' calculates the inverse of the special "Matrix" created with the above function.
# It first checks to see if the inverse has already been calculated
#   If so, it 'get's the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 'setInverse' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
        
}















