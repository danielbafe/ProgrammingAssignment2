##  These two pair of functions can cache the inverse of a matrix

##  This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The second function computes the inverse of the matrix create above. If the
## first  function has been executed, this one should retrieve the inverse from
## cache. the first line guarantee that de inverse has been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached data!")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
  
}
