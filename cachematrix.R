## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This method accepts a matrix as an argument
# It returns a list of functions which operate on the passed matrix.
# These methods can set/get the matrix value itself as well as the inverse of the matrix
# it caches the value of inverse of the matrix in the variable 'i'
# Note: This must be used to create a cached matrix which can be passed as an argument to cacheSolve
##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(inv) { i <<- inv}
  getinverse <- function() { i }
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This method accepts a 'cached matrix' object as an argument i.e.
# a matrix which has been returned by the 'makeCacheMatrix' method.
# Given such a matrix, it returns it's inverse. The first time this 
# mthod is called for a new matrix, the inverse will not be available.
# Hence it is computed and then set in the cached matrix. 
# In subsequent calls the cached value will be non-null and returned directly.
# An example of how to use this
# ... for a matrix 'mymat'
# > cachedM <- makeCacheMatrix(mymat)
# invOfMyMat <- cacheSolve(cachedM)
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' - which is a 'cached Matrix'
    inv <- x$getinverse()
    # check if already computed
    if( !is.null(inv)){
      message("getting cached inverse of matrix")
      return(inv)
    }
    
    # get the matrix values
    data <- x$get()
    # compute the inverse
    inv <- solve(data)
    # cache the inverse value in the 'cachedMatrix' x
    x$setinverse(inv)
    # return the inverse value
    inv
}
