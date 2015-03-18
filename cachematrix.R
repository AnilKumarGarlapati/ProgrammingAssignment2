## Creating MakeCacheMatrix, that caches the matrix on calling it with a matrix as input.
## In this command "y<-makeCacheMatrix(x)" x is a inversiable matrix and y will contain 
## the list returned by makeCacheMatrix()
## function:: makeCacheMatrix() stores the inverse calculations in the R environment which will be 
## accesses by cacheSolve() and are displayed
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

## cacheSolve() function travers through the environment and get cthe cached results for you
## We may not understand this for small matrices but when a large matrix inversion is taking
## place, this helps get at the moment calculations and see how it is proceeding
cacheSolve <- function(x, ...) {        
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
