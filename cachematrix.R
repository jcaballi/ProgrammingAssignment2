## makeCacheMatrix makes a matrix that can cache it's inverse
## cacheSolve returns the inverse of the matrix x

## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Returns inverse of matrix x; from cache if it's already been
## computed, otherwise computes it using solve()

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     i <- solve(x$get())
     x$setinverse(i)
     i
}
