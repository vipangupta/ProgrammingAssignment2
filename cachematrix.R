# We use the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.

## The function creates a list with four functions
## i) to set value of matrix, ii) get value of matrix
## iii) set value of inverse, iv) get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## initialize to null
  ## set function:: 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## return the input matrix
  setInverse <- function(inverse) inv <<- inverse ## set the inverse matrix
  getInverse <- function() inv  ## return the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve calculates the inverse of the matrix: It checks if value is previously calculated
## if true -- returns the previously calculated value, else calculates the value of the inverse
cacheSolve <- function(x, ...) {
  o <- x$getInverse()  ## get the inverse matrix-- will be null if not previously calculated
  if(!is.null(o)) {
    message("previously calculated data exists - returning cached values")
    return(o) ## return the previously computed inverse
  }
  data <- x$get() ## no previously calculated inverse exists; let's get the matrix
  o <- solve(data, ...) ## calculate the inverse
  x$setInverse(o) ## set value of object -- inv is no longer null.
  o ## return the result.
}