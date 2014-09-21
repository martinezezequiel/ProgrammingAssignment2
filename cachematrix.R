## This code allows to save computational time when finding the inverse of a matrix
## using the SOLVE function. The first time the inverse gets calculated the value gets
## cached and then everytime that inverse is requied the program will pull the data from
## cache instead of calculating again.

## This function generates a vector or list of functions.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s<<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function performs two tasks, it calculates the inversa value of a Matrix using the solve
## function or returns the value of the inverse stored in cache in the case where the inverse
## has been previously calculated.

cacheSolve <- function(x, ...) {
        
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
