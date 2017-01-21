## The cacheSolve takes a parameter of the matrix created by the
## makeCacheMatrix function and gets the inverse either from the cache
## or creates the inverse and saves it in the cache.


## The makeCacheMatrix is a function to take a simple matrix as a parameter
## and return list of functions to get and set the matrix 
## and get and set the matrix inverse from/in the cache
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a matrix created from the above function as its parameter, 
## and returns its inverse. 
## It first finds the inverse in the cache; if found returns the value from cache
## else, creates the inverse and saves it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!isnull(inverse)) {
    message("matrix inverse is cached; returning value")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$setinverse(inverse)
  inverse
}
