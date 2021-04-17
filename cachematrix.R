## With this pair of functions you can calculate the inverse of a matrix, stored it in
## the cache and then call the result skiping the computation.

## The function makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## First you assign the function to an element, using a matrix as an argument
## (e.g. tm <- makeCacheMatrix(matrix(1:4, 2, 2))).
## After that, you can get the matrix printed by typing tm$getinvm()
## In case the inverse of the matrix has already been calculated, you can get
## it by writting tm$getinvm(). If not, the result would be NULL.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inverse) invm <<- inverse
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm, getinvm = getinvm)
}

## This function calculates the inverse of the matrix, returning a matrix thats is the
## is the inverse of x. But first, it checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation (showing the
## message gettin cache data). If not, it calculates the inverse of the data and sets
## the value in the cache via the setinvm function.

cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if (!is.null(invm)) {
    message("getting cache data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}
