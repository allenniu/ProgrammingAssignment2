## Put comments here that give an overall description of what your
## functions do 
## show whether the reverse matrix from cash or not
## to test it
##
##> y<-matrix(runif(9), 3, 3)
##> y_m<-makeCacheMatrix(y)
##> cacheSolve(y_m)
##> cacheSolve(y_m)    --- should show 'getting cached data 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_solve <- function(p_s) m <<- p_s
    get_solve <- function() m
    list(set = set, get = get,
         set_solve = set_solve,
         get_solve = get_solve)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_solve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_solve(m)
    m
  }
