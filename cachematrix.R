##Assuming it's always given square matricies, the makeCacheMatrix function creates a matrix and caches its inverse
##"set" sets the value of the matrix
##"get" gets the value of the matrix
##"setInverse" creates an inverse of the matrix
##"getInverse" retreives the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##"cacheSolve" gets the inverese from cache if already done, or computes the inverse matrix if not already done. 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("work in progress")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}