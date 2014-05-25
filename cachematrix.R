## Functions that are used to create a special object that stores a invertible matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse
  setsolve <- function(solve) m <<- solve
  
  # Get the value of the inverse
  getsolve <- function() m
  
  # Return a list of function to set and get value of the matrix and inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  # Get inverse of x from cache
  m <- x$getsolve()
  
  # If inverse from cache is not null return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Get the value of the matrix
  data <- x$get()
  
  # Calculate inverse of the matrix
  m <- solve(data, ...)
  
  # Set the value of the inverse
  x$setsolve(m)
  
  # Return inverse
  m
}
