## Functions create a list representing a cachable matrix and its inverse and
## takes that list and returns the inversed matrix if it is already cached
## or calculates, caches, and returns the inversed matrix if it is not already cached

## makeCacheMatrix is used by cacheSolve to hold a cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    
    }
  
    get <- function() x
  
    setInverse <- function(inverse) i <<- inverse
  
    getInverse <- function() i
  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inversed matrix if it is cached or calculates,
## caches, and returns the inversed matrix if it is not already cached

cacheSolve <- function(x, ...) {
    ## Get inverse
    i <- x$getInverse()
  
    ## If inverse is not NULL, return cached inverse
    if(!is.null(i))
    {
        return(i)
    }
    ## Else get matrix then calculate, cache, and return inverse
    else
    {
        m <- x$get()
        i <- solve(m)
        x$setInverse(i, ...)
    }
  
    i
}
