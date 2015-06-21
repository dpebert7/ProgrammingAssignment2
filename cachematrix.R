## This R code is a submission for Programming Assignment 2

## THe following function stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
      
}


## The following function returns a matrix's cached (or
## else calculated) inverse

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}