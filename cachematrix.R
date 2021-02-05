## A pair of functions that cache the inverse of matrix

## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       # Initiate the inverse property
       i <- NULL
  
       ## Set the matrix
        set <- function(matrix) {
         m <<- matrix
          i <<- NULL
       }
        ## Get matrix
       get <- function() {
       ## Return the matrix
         m
       }
  
        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
         i <<- inverse
    
        }
         ## Get the inverse of the matrix
        getInverse <- function() {
        ## Return the inverse property
        i
        }
  
        ## Return a list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above
## If the inverse has already been calculated, then the "cacheSolve" should 
## retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
        return(m)
        }
  
        ## Get the matrix from object
        data <- x$get()
  
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
  
        ## Set the inverse to the object
        x$setInverse(m)
  
        ## Return the matrix
        m
}