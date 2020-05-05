## The first two functions, create a special object, that
## stores a matrix and then caches its inverse

## MakeCacheMatrix, creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
   
   i <- NULL
   
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## caheSolve, produces the inverse of the special matrix
## created by MakeCacheMatrix

cacheSolve <- function(x, ...) {
   
   i <- x$getinverse()
   
   if (!is.null(i)) {
      message("getting data from cache")
      return(i)
   }

   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i

}

## Source this program and call run this function is
## just a sample "keyboard shortcut", but you may try
## MakeCacheMatrix & cacheSolve with your own data

run <- function() {
   
   x <- matrix( rnorm(20), 4,4)
   y <- makeCacheMatrix( x )
   cacheSolve( y )
   
}