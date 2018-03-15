## The goal of these two functions is to create a method for storing the inverse of
## an invertible square matrix in the cache, so we can call it each time it is
## required, rather than computing the inverse from scratch each time.

## The function makeCacheMatrix creates a list of functions that: get or set the  
## value of the input matrix, or get or set the inverse matrix of this input.

## Note: the output of makeCacheMatrix is what we feed into the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The function cacheSolve takes as input the list ouputted by makeCacheMatrix, and
## checks if the original input matrix has already had its inverse found. If so, it
## reads this inverse from the cache; otherwise, the inverse is explicitly found,
## and added to the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
