## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix()) {
  
  ## Initialize the inverse matrix
  mi <- NULL
  
  ## Function to set the matrix
  set <- function( y ) {
    x <<- y
    mi <<- NULL
  }
  
  ## Function the get the matrix
  get <- function() {
    x
  }
  
  ## Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    mi <<- inverse
  }
  
  ## Function to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse of the matrix
    mi
  }
  
  ## Finally return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by the function "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## m will be a matrix inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse (m) if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## Calculate the inverse using solve(data, ...)
  m <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Finally return the inverse matrix 
  m
}
