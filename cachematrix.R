## The R script contains two functions, "makeCacheMatrix" and "cacheSolve"
## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## Function "cacheSolve" This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## 
## A simple example:
##   x<-matrix(1:4, nrow=2, ncol=2); invs<-solve(x)
##   xx1<-makeCacheMatrix()
##   xx1$set(x)
##   xx1$get()
##   cacheSolve(xx1)   # no cached inverse
##   xx1$setinverse(invs)
##   xx1$getinverse()
##   cacheSolve(xx1)   # cached inverse and matrix has not changed
##   xnew<-matrix(c(1:3,4.01), nrow=2, ncol=2)  # new matrix x, but invs keeps same
##   xx1<-makeCacheMatrix()
##   xx1$set(xnew)
##   xx1$get()
##   cacheSolve(xx1)   # no cached inverse
##   xx1$setinverse(invs)
##   xx1$getinverse()
##   cacheSolve(xx1)   # cached inverse but matrix has changed, so the inverse has to be re-calculated
##

## This function creates a special "matrix" object that can cache its inverse
## It contains a list of functions
## 1. "set": set the value of the matrix
## 2. "get": get the value of the matrix
## 3. "setinverse": set the inverse of the matrix
## 4. "getinverse": get the inverse of the mean


makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL # inverse of matrix
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse()
  if(!is.null(invs)) {
    # if the inverse has been cached, check whether inverse corresponds to matrix x (i.e. whether matrix has changed)
    if(dim(invs) == dim(x$get()) && all(invs %*% x$get() == diag(dim(invs)[1]))) {
      message("getting cached inverse")
      return(invs)  
    } else {
      message("matrix has changed! re-caculate the inverse...")
    }  
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}