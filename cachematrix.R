#' **ASSIGNMENT 2**
#'
#' These two functions will operate to calculate the inverse of a matrix - this 
#' is usually a computationally costly exercise, and so writing functions can be hugely beneficial. 
#' 
#' There will be two functions:
#' 
#' 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#' 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the 
#' inverse from the cache.
#'
# makecachematrix
makeCacheMatrix <- function(x = matrix()) {
  # intialise the inverse matrix
  inverse <- NULL 
  # set function for the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get function of the matrix - define the return matrix object
  get <- function() {
    # return special matrix
    x
  }
  # need to set the inverse matrix
  setinverse <- function(invM){
    # assign the inverse matrix to the environment
    inverse <<- invM
  }
  # need to define the function for returning the inverse matrix
  getinverse <- function(){
    inverse
  } 
  # create a list with all the defined functions for the environment
  list( set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#' This function will: 
#' 1. Check if the inverse object exists and the matrix is unchanged by 
#' getting the stored inverse matrix calling from chachematrix function
#' 2. Return the inverse object in cahce if it satisfies the first rules.
#' 3. Else, calculate the inverse of the special matrix and store this in cahcematrix and return 
#' the new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the inverse of the original matrix produced in cahcematrix
  invM <- get$getinverse
  # determine if the inverse has already been calculated
  if(!is.null(invM)){
    # need to check that the inverse is identical
    if(identical(x$get() %*% invM, invM %*% x$get() )) {
      # skip the computation by getting it from cache
      message("Getting Cached Data")
      return(invM)
    } 
  }
  # else calculate the inverse
  data <- x$get
  inverse <- solve(data, ...)
  # set the value of the inverse in the cache using setinv
  x$setinverse(invM)
  print("Getting New Computed Data")
  return(invM)
}
