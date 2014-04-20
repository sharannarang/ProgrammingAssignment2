## makeCacheMatrix saves a matrix, its inverse and number of cache hits. 
## cacheSolve checks if the inverse of a matrix is present in the cache and if not it computes the inverse and saves it.
## Subsequent calls to cacheSolve will retreive the answer from the cache. 

## Returns a list of functions that can be used to:
## - set a new matrix (or inverse) 
## - get the matrix or inverse
## - get the number of hits in the cache
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse to NULL at the beginning. 
    inverse <- NULL
    ## Initialize hits to zero at the beginning. 
    hits <- 0
    ## setMatrix saves a new matrix.
    ## The inverse is set to NULL since we haven't computed it before.
    setMatrix <- function(m) {
        x <<- m
        inverse <<- NULL
        hits <<- 0
    }
    ## Just returns the matrix that is saved. 
    getMatrix <- function() {
        x
    }
    ## Takes an arguement and sets it to the inverse of the matrix.
    setInverse <- function(inv) {
        inverse <<- inv
    }
    ## Returns the inverse without any computation.
    getInverse <- function() {
        ## if inverse is not null, we hit in the cache and increment the hits.
        if (!is.null(inverse)) {
            hits <<- hits + 1
        }
        inverse
    }
    ## Returns the number of hits. 
    getHits <- function() {
        hits
    } 
    ## Return a list with the required functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse, getCacheHits = getHits)
}


## The function is used to compute the inverse of a matrix and save the result in a cache. 
## Once the result is saved subsequent calls will access the cached result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    answer <- x$getInverse()
    ## If the answer exists in the cache, we're done! 
    if (!is.null(answer)){
        print("Returning cached result")
        answer
    }
    ## Else compute the answer and return it
    else {
        ## Get the saved matrix.
        matrix <- x$getMatrix()
        ## Compute the inverse.
        answer <- solve(matrix , ...)
        ## Save the result in the cache.
        x$setInverse(answer)
        answer
    }
}
