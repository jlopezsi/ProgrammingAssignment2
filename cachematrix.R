

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix creates a list containing a function to
## Put comments here that give an overall description of what your
## functions do
# 1. set the value of the matrix  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
# 2. get the value of the matrix
  get <- function() x
# 3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
# 4. get the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  # The following function returns the inverse of the matrix. It first checks if
  # the inverse has already been computed. If so, it gets the result and skips the
  # computation. If not, it computes the inverse, sets the value in the cache via
  # setinverse function.
  # This function assumes that the matrix is always invertible.
  
  #Gets inverse from cache
  inv <- x$getinverse()
  #if it exists, then  get inverse fom cached data
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  #If not, then solve matrix inverse
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
