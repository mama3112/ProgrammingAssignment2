
makeCacheMatrix <- function(x = matrix()) { 
  ## initialize the value of the matrix inverse to NULL
  matrixinverse <- NULL                     
  ## delcare another function set where the value will be cached in 1. Matrix is created
  ## for the first time. 2. changes made to cached matrix
  set <- function(y) {                      
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
    matrixinverse <<- NULL              
  }
  ## gets the value of the inverse
  get <- function() x                           
  #calculates the inverse of non-singular matrix via the solve function
  setinverse <- function(solve) matrixinverse <<- solve 
  # gets the inverse     
  getinverse <- function() matrixinverse        
  ## passes the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

# used to get the cache of the matrix
cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  #if the inverse exists, it gets it.
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #if the inverse if not there, first it is calculated and then retrieved.
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
