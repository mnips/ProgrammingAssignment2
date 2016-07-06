#The function creates a matrix that can cache its inverse.
#getInverse helps in getting cached data
#set and get used to assign and retrieve data
#While setinverse sets the inverse for caching
makeCacheMatrix <- function(a = matrix()) {
    inv <- NULL
    set <- function(b) {
        a <<- b
        inv <<- NULL
    }
    get <- function() a
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The give function below returns a matrix that is the inverse of 'x'
## If the inverse has already been calculated then it retrieves the inverse from the cache.
cacheSolve <- function(a, ...) {
    
    inv <- a$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- a$get()
    inv <- solve(mat, ...)
    a$setInverse(inv)
    inv
}
