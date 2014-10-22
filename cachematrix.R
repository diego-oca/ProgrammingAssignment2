## Put comments here that give an overall description of what your
## functions do

## Returns a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set method (stores given value and clears the matrix inverse value)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get method (simply returns the value)
  get <- function() x
  # setinverse method (stores the calculated value for later use)
  setinverse <- function(inverse) m <<- inverse
  # getinverse (returns cached value, or null)
  getinverse <- function() m
  #return the cacheable matrix with its methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solves matrix inversion using a cacheable matrix

cacheSolve <- function(x, ...) {
  # Get cached value (if any)
  m <- x$getinverse()
  if(!is.null(m)) {
    # If a cached value exist, we simply return it
    message("getting cached data")
    return(m)
  }
  # If there is no cached value available, we solve the matrix inverse...
  data <- x$get()
  m <- solve(data, ...)
  #... and store it as a cached value
  x$setinverse(m)
  # Then, we return the value we have just calculated
  m
}
