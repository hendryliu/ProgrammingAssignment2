## Put comments here that give an overall description of what your
## functions do
## The R script contains two functions, "makeCacheMatrix" and "cacheSolve"
## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## Function "cacheSolve" This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## It contains a list of functions
## 1. "set": set the value of the matrix
## 2. "get": get the value of the matrix
## 3. "setinverse": set the inverse of the matrix
## 4. "getinverse": get the inverse of the mean


makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse()
  if(!is.null(invs) & ) {
    message("getting cached inverse")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}