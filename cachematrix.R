## An example of caching an expensive computation

## Store a matrix with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  # take a matrix x
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  # return a list of setters and getters for the matrix and its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the matrix and cache it,
## if it is not yet computed; otherwise, return the
## cached inverse

cacheSolve <- function(x, ...) {
  # take an object constructed by makeCacheMatrix
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  # return the inverse of the matrix
  m
}
