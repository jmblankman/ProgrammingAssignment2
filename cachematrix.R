## Program description:

## The makeCacheMatrix function creates the matrix and commands
## that the inverse be cached. 

## The cacheSolve function checks to see if the inverse has
## been previously calculated and either returns the previous
## solution or completes the initial inverse computation. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function (makeCacheMatrix) creates a matrix that caches its inverse 
## via functions that 'set' and 'get' the values of both 
## the matrix and its inverse.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## This function (cacheSolve) first determines whether or not 
## the inverse of a matrix has been previously calculated.
## If it has, the function returns the cached solution. 
## If it has not, the function calculates the inverse.
