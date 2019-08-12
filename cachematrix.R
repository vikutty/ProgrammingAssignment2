## Program for

## makeCacheMatrix function records and caches the inverse of the matrix provided
## Assumption is the matrix provided is inverse compatible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function() {
    m <<- solve(x)
  }
  getinv <<- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  setinv()
  # getinv()
}

## cacheSolve function tries to capture the inverse of matrix from makeCacheMatrix
## If value not available then the function generates the inverse based on input variable

cacheSolve <- function(x, m, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <<- getinv()
  }
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else {
    message("getting the inverse of the matrix")
    data <- x$get()
    i <- solve(data)
    return(i)
  }
}
