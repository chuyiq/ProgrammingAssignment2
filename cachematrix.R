## Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse

# "makeCacheMatrix()" creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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

# "cacheSolve()" function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

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

# Sample Run:
# > x <- matrix(c(1,2,3,4,5,6,7,8,0),3,3)
# > inv <- makeCacheMatrix(x)
# For the first run, returned the calculated inverse
# > cacheSolve(inv)
#            [,1]       [,2]       [,3]
# [1,] -1.7777778  1.5555556 -0.1111111
# [2,]  0.8888889 -0.7777778  0.2222222
# [3,] -0.1111111  0.2222222 -0.1111111
# For the second run, retrieve the cached inverse
# > cacheSolve(inv)
# getting cached data
#            [,1]       [,2]       [,3]
# [1,] -1.7777778  1.5555556 -0.1111111
# [2,]  0.8888889 -0.7777778  0.2222222
# [3,] -0.1111111  0.2222222 -0.1111111
# The inverse returned by "solve()" function
# > solve(x)
#            [,1]       [,2]       [,3]
# [1,] -1.7777778  1.5555556 -0.1111111
# [2,]  0.8888889 -0.7777778  0.2222222
# [3,] -0.1111111  0.2222222 -0.1111111

