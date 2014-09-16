## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse
    inv <- NULL
    ## set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the matrix
    get <- function() x
    ## set inverse of a matrix
    setinverse <- function(inverse) inv <<-inverse
    ## get the inverse of a matrix
    getinverse <- function(inverse) inv
    ## return a list 
    list(set=set, get=get,
         setinverse = setinverse
         getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## check if inverse of x is already set, if yes return it
    if (!is.null(inv)) {
        nessage("getting cached data")
        return(inv)
    }
    ## get matrix from x
    data <- x$get()
    ## calculate the inverse of the matrix
    inv <- solve(data)
    ## set the inverse to x
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
