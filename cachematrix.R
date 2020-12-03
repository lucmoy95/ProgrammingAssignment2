## This is a series of functions that allows the user to cache potentially 
## time consuming computations to be retrieved at a later time.

## This function is essentially four functions nested within the function,
## "makeCacheMatrix". The code creates a special vector which, allows the
## user to 1)set the value of matrix, 2)get the value of the matrix, 3)set 
## the value of the inverse of the matrix, and 4) get the value of the 
## inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function determines whether there is a cached inverse value for the 
## matrix used in the function, "makeCacheMatrix". If there is an inverse
## value, the function would print a message "getting cached data" and retrieve
## the inverse value from the function above. If there is no cached inverse 
## value, this function computes the inverse value for said matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
