## These functions used in combination help convserve resources by
## caching the inverse of a matrix for re-use instead of recomputation
## First pass your square numeric matrix into makeCacheMatrix, store it in a variable
## Pass the variable into cacheSolve to receive the inverse of the matrix.

## makeCacheMatrix will set and get the value of matrix from another 
## environment so that the value will remain in memory for the next
## call of the function

makeCacheMatrix <- function(x =numeric()) {
    ## Clear the initial value
    m <- NULL
    set <- function(NewVar) {
        x <<- NewVar
        ## New Matrix entered, so flush the cache
        m <<- NULL
    }
    ## get the matrix back
    get <- function() {
        x
    }
    setsolve <- function(solve) {
        ## Tell it what you want to do to the Matrix    
        m <<- solve
    }
    ## Get the result and return the list
    getsolve <- function(){
        m
    }
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## cacheSolve will Solve a matrix for its inverse, but will first check the cache to see if
## the inverse already exists to use instead.  If it does need to calculate it will
## save the matrix inverse into cache for the next call.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Get the cached Value
    m <- x$getsolve()
    if(!is.null(m)) {
        ## if the matrix has already been solved, return the cached value
        message("getting cached data")
        return(m)
    }
    ## if the matrix has not been solved, do so and store in the cache before returning
    data <- x$get()
    m <- solve(x)
    x$setsolve(m)
    m
    
}
