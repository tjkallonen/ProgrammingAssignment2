## Put comments here that give an overall description of what your
## functions do

## A function for creating a matrix object with a possibility to cache the inverse of it
## Parameter x = matrix to be inverted
## Returns a matrix "object" (really a list of functions tied to a matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## The inverse value
        ##Set the object to point to a new matrix and zero the inverse
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x ## Get the matrix
        setinverse <- function(inverse) m <<- inverse ## Set the inverse of the matrix
        getinverse <- function() m ## Get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## A function for solving the inverse of a matrix 
## Parameter x = matrix object to be inverted
## Returns the inverse of x, either by solving it or retrieving it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## Check if the inverse has been calculated already and return it if it has
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ## If there is no data in cache, solve the inverse, store the result in cache and return the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
