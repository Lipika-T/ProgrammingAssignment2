## The following functions calculate the inverse of a matrix and cache it, and if required, 
## retrieve the inverse from the cache.
## This makes matrix inversion (which is normally computationally costly) more efficient.

## The following function makeCacheMatrix() creates a special kind of matrix whose inverse can be cached.
## It returns a list object whose elements are functions to set and get the values of the matrix and its inverse.
## Everytime a new matrix is passed as an argument, the inverse (i) is reset to NULL. 

makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## The function takes as an argument, an object returned by makeCacheMatrix.
## If the matrix being passed is a new one (its inverse is NULL), its inverse is calculated, 
## returned and also passed to setInv() where it's stored.
## If the matrix is one whose inverse had already been calculated, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInv(i)
  i
}