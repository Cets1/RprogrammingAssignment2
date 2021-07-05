## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

# 1. set the value of the matrix
  set <- function(y) {
    x<<- y
    inv <<- NULL
  }
# 2. get the value of the matrix
  get <- function () x
# 3. set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
# 4. get the inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function (x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
