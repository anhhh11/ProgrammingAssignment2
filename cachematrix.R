## Create data str
## functions do

## Create a wrapper of matrix which have functions to set and get 
## its value and value of inversed ones

makeCacheMatrix <- function(x = matrix()) {
  #Init
  val <- x
  myInversed  <- NULL
  #Get value func
  get <- function() val
  #Set value func
  set <- function(val) {
    x <<- val
  }
  #Get cached inversed matrix 
  getInversed <- function() myInversed
  #Set cached inversed matrix
  setInversed <- function(inversedMatrix) myInversed <<- inversedMatrix
  #functions forward to outside
  list(get=get,set=set,
       getInversed=getInversed,setInversed=setInversed)
}


## return or (Compute, save, return) cached inversed matrix of cache matrix
cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInversed()
  ## Return value if cache hit
  if (!is.null(inversedMatrix)){
    message("getting cached data")
    return(inversedMatrix)
  }
  ## Compute if cache is empty
  origMatrix <- x$get()
  inversedMatrix <- solve(origMatrix)
  x$setInversed(inversedMatrix)
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix
}
