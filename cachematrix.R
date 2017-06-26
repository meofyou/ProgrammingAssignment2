## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function for making a Matrix which can cache the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Function for solving the inverse of Matrix using makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("gettting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
