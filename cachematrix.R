## Below R functions are able to "cache" calculated matrix inversions.The functions 
## utilize lexical scoping rules which allow a matrix input and it's solved inverse
## to be stored in an environemnt that is separate from the current working environment.
## This allows the user to retrieve the inverse matrix for a set matrix input multiple
## times (i.e. from cache) without having to recaclulate the inverse each time in memory. 

## The function "MakeCacheMatrix()" takes an input matrix as the argument. 
## The function then creates a list object of other functions which allow the user to
## retrieve the input matrix, re-set the input matrix, set the matrix inverse, or retrieve
## the matrix inverse if previously set or solved for (see cachesolve() funtion).
## The named list functions are based on objects maintained within the 
## makeCacheMatrix() environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function "cachesolve()" takes an argument of the type "makeCacheMatrix()" above.
## Based on the set matrix input from makeCacheMatrix(), this function will solve
## the matrix inversion if it hasn't already been done and cache in the 
## makeCacheMatrix() list.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}
