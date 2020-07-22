## Assignment: Caching the Inverse of a Matrix (Function)

## First, this function creates a special "matrix" object that can cache its inverse.

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

## Now, the catcheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i     
}
