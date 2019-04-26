## Caching the Inverse of a Matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
                x <<- y
                inv <<- NULL
        }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Retrieve the cache or calculate the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinv(inv)
      inv
}
