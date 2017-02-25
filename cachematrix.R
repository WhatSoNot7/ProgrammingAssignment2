## Matrix inversion

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverse()
  if (!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  mat <- x$get()
  inv_matrix <- solve(mat, ...)
  x$setInverse(inv_matrix)
  inv_matrix
}
