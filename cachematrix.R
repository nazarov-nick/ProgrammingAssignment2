## R function is able to cache potentially time-consuming computations
## functions below inverse matrix, it may be helpfull if matrix has a large size

##  function makeCacheMatrix() Creates an object that is a matrix 
## with a set of options

makeCacheMatrix <- function(x = matrix()) {
 
  InvM <- NULL # initialization
 
  # change matrix  'x'
  set <- function(y) {
    x <<- y
    InvM <<- NULL
  }
  
  #return a matrix  'x'
  get <- function() x
  
  # change inversed  'x'
  setInversion <- function(InvMatrix) InvM <<- InvMatrix
  
  #return a matrix that is the inverse of matrix  'x'
  getInversion <- function() InvM
  
  #return a list of options
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)

}


## function cacheSolveb () takes as argument an object makeCacheMatrix and return 
## inverse of matrix that was contained in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvM <- x$getInversion()
 
  if(!is.null(InvM)) {
    message("getting cached data")
    return(InvM)
  }
 
  #inverse of matrix  'x' for the first time
  data <- x$get()
  InvM <- solve(data)
  x$setInversion(InvM)
  InvM
   
}
