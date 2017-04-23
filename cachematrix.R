## These functions first set the value of a matrix and then set the value of a matrix
## These functions then compute the inverse of a matrix and set that value as well

## cashe a matrix because this can be a resource intensive process and we don't
## want to do it over and over 

makeCacheMatrix <- function(cashed_matrix = matrix()) {
  inverse <- NULL
  set <- function(internal_function_matrix) {
    ## modify the variable at the parent level
    cashed_matrix <<- internal_function_matrix
    inverse <<- NULL
  }
  get <- function() cashed_matrix
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(cashed_matrix, ...) {
  ## Return a matrix that is the inverse of 'cashed_matrix'
  inverse <- cashed_matrix$getinverse()
  ## if the inverse is not NULL then jsut return the inverse 
  if(!is.null(inverse)) {
    message("There is cashed data, so it will be used")
    ## the return ends the funciton so we skip the inverse work below
    return(inverse)
  }
  data <- cashed_matrix$get()
  inverse <- solve(data)
  cashed_matrix$setinverse(inverse)
  inverse
}
