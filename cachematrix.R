## The  function accepts an invertible matrix and  creates a special vector of length 4 functions.
## Input  : an invertible matrix
## Output  :  creates a special "vector", which is really a list containing a function to
                set the value of the matrix
                get the value of the matrix
                set the value of the inverse of the matrix
                get the value of the inverse of the matrix

makeCacheMatrix <- function(x) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



 ## Calculates the inverse when called 1st time and also caches it. Subsequent calls  gets the result from cache.
 ##Input : a spcial vector created by function make cacheMatrix
 ##Output : inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Example
## m<- matrix(1:4,2,2)
## sm <- makeCacheMatrix(m)
## cacheSolve(sm)
