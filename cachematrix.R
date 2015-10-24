## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly (there
## are also alternatives to matrix inversion that we will not discuss here).
## The following functions are used to create a special "matrix" and caches its
## inverse

## Creates a special "matrix", which is really a list containing a function to
## 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been computed. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it computes the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function. This
## function assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
