## The following set of functions is designed to speed up the calculation of
## the inverse of a matrix, if it has to be done repeatedly. It does so using
## the principle of lexical scoping.

## This function creates a special matrix object, which is really a list 
##containing a function to:
##
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the mean
## has already been calculated. If so, it gets the mean from the cache and 
## skips the computation. Otherwise, it calculates the mean of the data and 
## sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
