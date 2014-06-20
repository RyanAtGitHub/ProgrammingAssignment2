## The makeCacheMatrix creates a list of subfunctions to store and give
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- matrix(0)                ## initialise inverse value
  
  setmatrix <- function(y) {        ## set matrix to be inversed
    x <<- y                             ## x is the matrix to be inversed
    inverse <<- matrix(0)                ## initialise inverse value
  }
  
  getmatrix <- function() x         ## output matrix to be inversed
  
  setinverse <- function(inv) inverse <<- inv   ## input inverse
  
  getinverse <- function() inverse          ## output inverse
  
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns inverse from cache or calculate it if it is not available

cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()    ##get calculated/cache inverse, if available, from makeCacheMatrix list
  
  if (inverse != matrix(0)) {   ##if inverse exist then execute
    message("getting cached data")
    return(inverse)
  }                 ##else execute below
  data <- x$getmatrix()   ##get matrix from makeCacheMatrix list
  inverse <- solve(data, ...)
  x$setinverse(inverse)  ##export calculated inverse to makeCacheMatrix list as cache
  inverse            ##return inverse
}
