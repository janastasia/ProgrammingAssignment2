## The following functions are used to cache the inverse (y) of a matrix (x)
## in order to avoid having to recompute (y), unless the original matrix (x)
## has changed.

## This function allows us to create a cached inverse matrix. These four functions
## allow us to modify (set) or get (get) the value of the matrix that is to be
## inverted (x) and to recalculate (setinv) or get (getinv) the value of the inverse (y).
## After the value of the function to set the value of (x) is called, the matrix (y) is
## no longer correct so we set it to NULL.

makeCacheMatrix <- function(x = matrix()) {
  
  y <- NULL
  set <- function(a)
  {
    x <<- a
    y <<- NULL
  }
  get <- function() x
  setinv <- function(a) y <<- a
  getinv <- function() y
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will return the inverse of the matrix that was created with
## the function above (makeCacheMatrix). If the inverse of (y) of (x) has not yet
## been calculated (it will be NULL), or the matrix (x) has recently changed
## (y will be NULL), then it recalculates the inverse of (x). Otherwise it 
## just returns the cached value of the inverse (x$getinv()).

cacheSolve <- function(x, ...) {
  y <- x$getinv()
  if (is.null(y)) {
    message("getting cached data...please wait")
    data <- x$get()
    y <- solve(data, ...)
    x$setinv(y)
  } 
  else {
    message("getting cached inverse...please wait")
  }
  return(y)
  
}
