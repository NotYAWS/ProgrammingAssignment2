## These functions are designed to achieve one thing; the 
## invesion of a matrix that can be effeciently recalled.

## This function uses the solve() function to calculate 
## the inverse of a given matrix, passed as x. This function
## uses nested functions and uses the lexical scoping concept
## of the R language to store the variables at the level above
## the function it is called in.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   ## a nested function
    x <<- y              ## assigning variables to the environment
    m <<- NULL           ## above the env. of the nested function
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, ## creates a list of functions to be 
       setsolve = setsolve,  ## consumed by the next function
       getsolve = getsolve)
}


## This function is the function to be called from the terminal. It
## Will first check to see whether the solve() function has already
## been performed, and will return the value already saved if it has.
## If it has not (that is, if is.null(m) is true), it invokes the
## nested functions from the previous function to create the matrix
## inversion.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {    ## If there has already been a matrix inversion
    message("getting cached data")  ## performed...
    return(m)                      ## return the saved matrix
  }
  data <- x$get()
  m <- solve(data, ...)           ## run the solve() function to invert
  x$setsolve(m)                   ## the matrix m
  m
}

