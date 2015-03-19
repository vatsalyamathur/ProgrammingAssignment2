## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  if (!is.matrix(x))
  {
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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        dimMatrix<-dim(m$get())
  if (!identical(dimMatrix[1], dimMatrix[2]))
  {
    print("nrow != ncol")
    print("Matrix should be square")
    return(NULL)
  }
  i <- m$getinverse()
  if(!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
