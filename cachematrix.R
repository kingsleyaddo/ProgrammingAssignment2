## This first part creates a special matrix object that can cache its inverse.
## It is based on the example given for the assignment for this week.

makeCacheMatrix <- function(x = matrix()) {
    invno <- NULL
    set <- function(y) {
      x <<- y
      invno <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invno <<- inverse
    getinverse <- function() invno
    list(set = set,  get =  get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This part returns the inverse of the matrix. It first checks cache to see if 
## value already exist to make computation faster when the same values are entered

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invno <- x$getinverse()
    if (!is.null(invno)) {
      message("getting cached data")
      return(invno)
    }
    matdata <- x$get()
    invno <- inverse (matdata, ...)
    x$setinverse(invno)
    invno
}
