## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() mananges a object which is a cache 
## preserving a calculated value of the inverse matrix
## given by argument x (x must be a matrix)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## cacheSolve() gets a makeCacheMatrix object given by
## argument x then checks whether it has cached value
## of inverse matrix. It a cache exists, then 
## cacheSolve() just returns it.  Otherwise, cacheSolve()
## calculates the inverse matrix, stores its value to the 
## cache of the makeCacheMatrix object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
