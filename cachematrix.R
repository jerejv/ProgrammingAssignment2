## Functions create a list representing a cachable matrix and its inverse and
## takes that list and returns the inversed matrix if it is already cached
## or calculates, caches, and returns the inversed matrix if it is not already cached

## makeCacheMatrix is used by cacheSolve to hold a cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
	## Set matrix
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
  
	## Get matrix
    get <- function() x
  
	## Set inverse matrix
    setInverse <- function(inverse) i <<- inverse
  
	## Get inverse matrix
    getInverse <- function() i
  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inversed matrix if it is cached or calculates,
## caches, and returns the inversed matrix if it is not already cached

cacheSolve <- function(x, ...) {
    ## Get cached inversed matrix
    i <- x$getInverse()
  
    ## If cached inversed matrix is not NULL, return cached inversed matrix
    if(!is.null(i))
    {
        return(i)
    }
    ## Else get matrix then calculate, cache, and return inversed matrix
    else
    {
        m <- x$get()
        i <- solve(m)
        x$setInverse(i, ...)
		return(i)
    }
}
