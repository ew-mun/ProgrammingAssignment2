## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function will take in a matrix as input and caches this; it will also
## set the inverse cache to null. It will return a matrix that gives access to to the functions 
## within this function
## cacheSolve will take the the matrix returned my makeCacheMatrix and check is the inverse has been populated,
## if it does, this inverse matrix will be returned, otherwise the cached matrix will have its inverse
##calculated and returned

## Write a short comment describing this function
## The makeCacheMatrix will cache the input matrix and set the cache of the inverse matrix to Null.  
## 4 functions will be defined within this function to set and get the cached matrix and its inverse
## The return is a matrix that gives access to these functions

makeCacheMatrix <- function(x = matrix()) {
  invMtx <- NULL
  
  ## caches the matrix
  set <- function(mtx){
    x <<- mtx
    invMtx <<- NULL
  }
  
  ## returns the cached matrix
  get <- function(){
    x
  }
  
  ## caches the inverted matrix
  setInverse <- function(iMtx){
    invMtx <<- iMtx
  }
  
  ## returns the cached inverted matrix
  getInverse <- function(){
    invMtx
  }
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## Write a short comment describing this function
## The cacheSolve function takes in the matrix output from MakeCacheMatrix
## It checks if the inverse of the matrix passed into makeCacheMatrix has been cached, and returns this
## If this is null, it retrieves the cached matrix, calculates its inverse, caches this and returns the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMtx <- x$getInverse()
  if(!is.null(invMtx)){
    return(invMtx)
  }
  
  ## create inverse as this not yet cached
  inMtx <- x$get()
  invMtx <- solve(inMtx, ...)
  x$setInverse(invMtx)
  invMtx
  
}
