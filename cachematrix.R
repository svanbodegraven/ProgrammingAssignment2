## Pair of functions to cache the inverse of a matrix

## makeCacheMatrix caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    p <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
        p <<- NULL
    }
    
    get <- function() x
    
    previous <- function() p
    
    setinverse <- function(inverse, x) {
        i <<- inverse
        p <<- x
    }
    
    getinverse <- function() i
    
    list(set = set, get = get,
         previous = previous,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of a matrix

cacheSolve <- function(x, ...) {

    p <- x$previous()
    
    if (!is.null(p)) {
        if (p == x) {
            i <- x$getinverse()
            
            if(!is.null(i)) {
                message("getting cached data")
                return(i)
            }        
        }
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i, x)
    
    i
}