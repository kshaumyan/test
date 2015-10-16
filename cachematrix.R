# The following two functions calculate the matrix inverse
# Since caculating inverse of a matrix potentially time consuming operation will will cache the result.
# if we have to calulate inverse of a same matrix in a row, we will get result from cache instead of
# recalcuating the result.
# Usage:
# a <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2) - matrix
# b<-makeCacheMatrix(a) - special vector of
# cacheSolve(b) - this call first call calculate matrix inverse using solve function and stores result in cache.
# cacheSolve(b) - this second call and all the subsequent calls with get the result from cache that saves time.


# makeCacheMatrix creates a list of 4 functions.
#
# 1.  set value of matrix
# 2.  get value of matrix
# 3.  set value of the matrix inverse 
# 4.  get value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
  m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function calculates the matrix inverse of the special "vector"
# created with the above function. However, it first checks to see if the
# matrix inverse has already been calculated. If so return matrix inverse from the
# cache and skip the computation. Otherwise, it calculates the matrix inverse of
# the data and sets the value of the matrix inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
