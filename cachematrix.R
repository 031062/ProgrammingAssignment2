## This is an example of lexical scoping, and how it can be used when the function's
## environment is the body of another function.

## In the makeCacheMatrix function, the 4 functions set, get, setinverse and getinverse
## are defined, as well as a list containing the 4 return values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function checks whether the inverse of the matrix x 
## has already been calculated.
## If so, the cached inverse is returned, otherwise the inverse is
## calculated via the solve function and returned.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}