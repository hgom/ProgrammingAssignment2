## returns object with 4 functions to set/get matrix and set/get inverse matrix
## set: define new matrix x
## get: return current matrix x
## setinverse: set inverse matrix
## getinverse: returns inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set new matrix in the enclosing environment and reset inverse matrix in the enclosing environment
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # return matrix x
  get <- function() x
  # set inverse matrix in the enclosing environment
  setinverse <- function(inv) inverse <<- inv
  # return inverse matrix
  getinverse <- function() inverse
  
  # return object function list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function returns inverse matrix of matrix x
## if the inverse matrix is stored in the cache then the cached inverse matrix is returned 
## if the inverse matrix is not stored in the cache then the inverse matrix is calculated and stored in the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    # return cached inverse matrix
    message("get cached inverse")
    return(inverse)
  }
  # calculate inverse matrix and store in the cache
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
