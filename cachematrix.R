## Use a special matrix object to cache the results of matrix inversion, a
## potentially time-consuming operation.

## Create the special matrix object. Returns a list which contains function for
## setting and getting the value of the matrix, and functions for setting and
## getting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## The value of the inverse, NULL until we calculate it
  set <- function(y) {
    ## Set the value of the matrix.
    x <<- y ## Store the value in x, which will be matched in the parent scope
    i <<- NULL ## Inverse must be recalculated since the the matrix has changed
  }
  get <- function() x ## Just get x (matrix value) using lexical scoping
  setinverse <- function(inverse) {
    i <<- inverse ## Store the inverse in the parent scope
  }
  getinverse <- function() i  ## get i (inverse) using lexical scoping
  list(set = set, get = get,  ## package everything as a list and return
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()  ## See if we have already cached the result
  if(!is.null(i)) { ## If the cache is not NULL, then we have
     message("getting cached data")
     return(i)
  }
  data <- x$get()  ## Otherwise, get the data and invert
  i <- solve(data, ...)
  x$setinverse(i)  ## Store the inverse in the cache
  i  ## and return it
}

