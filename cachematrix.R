## In this assignment we will be writing a pair of functions that caches the inverse of a matrix
## for further use so the calculation does not need to be repeated

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse
## This function does 4 things:  
  ## sets and gets the value of the matrix
  ## sets and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Check if inverse already exists in cache, if so, return value
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #If inverse not already cached, take the inverse, cache and return
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  return(inv)
}
