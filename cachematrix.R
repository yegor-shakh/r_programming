
## $set() - stores a matrix in this envirement
## $get() - loads the matrix from this envirement
## $setsolve() - stores an inverted matrix to this envirement
## $getsolve() - loads the matrix from this envirement

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  # create a list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Load a stored inverted matrix or invert if it has not been stored and return

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


## Generate a square matrix with size of 'x'

generateMatrix <- function(x) {
  A <- stats::rnorm(x*x)
  dim(A) <- c(x,x)
  return(A)
}
