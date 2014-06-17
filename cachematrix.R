## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- NULL    
            ## Set the value of matrix
    set <- function(y) {
        message("Setting ", y)
        x <<- y
        inv <<- NULL
    }    
    ## Get the value of matrix
    get <- function() x          
    getInverse <- function() cachedMatrix        
    setInverse <- function(inverse_matrix) {
        cachedMatrix <<- inverse_matrix
    }    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## Create inverse of a matrix if its not avalable in the cache
cacheSolve <- function(a, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- a$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    # If not in the cache get the matrix x from makeCacheMatrix() 
    data <- a$get()    
    
    # Inverse it
    inv <- solve(data, ...)
    
    # Set the cache
    a$setInverse(inv)   
    inv
}
