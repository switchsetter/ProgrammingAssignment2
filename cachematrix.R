## pair of functions that cache the inverse of a matrix.
##test usage:
##  make a matrix
##   z<-c(1,3,3,1,4,3,1,3,4)
##   mt<-matrix(z,3,3)
##  pass it to makeCacheMatrix
##   matrix_obj<-makeCacheMatrix(mt)
##  get its inverse
##   cacheSolve(matrix_obj)
##  run it again, the function retrieves the inverse from the cache.



## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve(x)
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix function. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}

