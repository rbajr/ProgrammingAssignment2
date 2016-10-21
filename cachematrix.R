## R Programming Assignment 2: 
## This script contains 2 functions, makeCacheMatrix and cacheSolve

## makeCacheMatrix accepts a matrix (for this exercise the matrix must be square and invertible)
## and returns a list containing 4 functions which set/get the input matrix or its inversion
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL    ##holds the inverted matrix of x
  
  ##reinitializes the matrix
  setMat <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##returns the matrix
  getMat <- function() x

  ##sets the inverted matrix of x
  setMatInv <- function(invMatrix)  i <<- invMatrix
  ##returns the inverted matrix of x
  getMatInv <- function() i
  ##return a list containing all 4 functions
  list(setMat = setMat,
       getMat = getMat,
       setMatInv = setMatInv,
       getMatInv = getMatInv
       )
}


## cacheSolve accepts an object of type makeCacheMatrix
## it returns the inverted matrix by first checking the cache (via the getMatInv function);
## if the inverted matrix does not exist in cache, the inverted matrix is created, 
## stored and returned
cacheSolve <- function(x) {
  #get the inverted matrix
  i <- x$getMatInv()
  if(!is.null(i)) {
    message("getting inverted matrix from cache")
    return(i)
  }
  #if getMatInv returned NULL, calc the matrix inversion
  #store it an return
  data <- x$getMat()
  i <- solve(data)
  x$setMatInv(i)
  i
}
