## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : this function provides setters and getters for the matrix, as well as the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
        set <- function(y) {    
                ## setter function of the matrix; initializes 'i' to null
                x <<- y
                i <<- NULL
        }
        get <- function() x     ## getter function for the matrix 'x'
        setinverse <- function(solve) i <<- solve       ## sets i to the inverse of matrix 'x'
        getinverse <- function() i                      ## returns i, the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: this function caches the inverse of the matrix, computed by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(i)
        i
}
