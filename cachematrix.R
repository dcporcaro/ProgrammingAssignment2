## Write a pair of functions that cache the inverse of a matrix.  The two functions are makeCcheMatris and cacheSolve 

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## defines input to the function to be a matrix
        m <- NULL              ## creates the variable m in the environment of the makeCacheMatrix function
        set <- function(y) {  ## creates a new function that allows us to assign a new value to x
                x <<- y  ## assigns x to be y using the superassignment operator
                m <<- NULL  ## resets the value of m to NULL so it will recalculate when cacheSolve is called
        }
        get <- function() x  ## returns the original matrix
        setinverse <- function(solve) m <<- solve  ## creates the inverse of the matrix using the solve function
        getinverse <- function() m  ## the function that returns the inverse of the original matrix
        list(set = set, get = get,  ##  a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {  ## creates the function
        m <- x$getinverse()   ## sets m to be the inverse of the original function as calculated by makeCacheMatrix
        if(!is.null(m)) {   ## a condition that checks to see if the inverse already has been calculated
                message("getting cached data")  ## prints this message if the inverse has already been calculated
                return(m)  ## returns the cached calculation
        }         ## if the cached calculation does not exist, then we move on to the next part
        data <- x$get()  ## creates an object called data that retrieves the original matrix
        m <- solve(data, ...)  ## calculates the inverse using the solve function
        x$setinverse(m)  ## sets the newly calculated inverse
        m  ## returns the newly calculated inverse
}