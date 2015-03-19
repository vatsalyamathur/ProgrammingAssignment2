## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## makeCacheMatrix() defines a new kind of matrix which caches it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  if (!is.matrix(x))
  {
    ## if x is not a matrix Object
    ## exit and print appropriate message
    print("Enter Matrix Object as argument")
    return(NULL)
  }
  set <- function(y) {
    x<<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## cacheSolve() is used to cache the inverse value of the matrix defined through makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  dimMatrix<-dim(x$get())
  if (!identical(dimMatrix[1], dimMatrix[2]))
  {
    print("nrow != ncol")
    print("Matrix should be square")
    return(NULL)
  }
  i <- x$getinverse()
  ## check for cached value for inverse matrix
  if(!is.null(i)) {
  ## if cache value of inverse not null
    message("Getting Cached Data")
  ## return cached inverse matrix
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## return inverse matrix
  i
}
