## These functions are for the Week Coursera Data Science: R Programming
## Week 3 Assignment.  Completed 20-SEPT-2022; GitHub user: ktmartin709

## The function below creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {        ## create blank "matrix" to house the functions
  inv <- NULL                                      ## create inv as NULL so that it can later hold the inverse values
  set <- function(y) {                             ## define the set function
    x <<- y                                        ## value of the matrix in parent environment
    inv <<- NULL                                   ## if there is a new matrix, reset inv to NULL
  }
  get <- function()x                               ## define the get function - returns the value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ##assigns the value of inv in parent evironment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## used to refer to the functions with the $ operator
}


## This function calculates the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
