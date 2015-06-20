## Seeing as to how matrix inversion is usually a costly (relatively slow) computation,
## we can potentially benefit from caching the inverse of matrix in order to avoid having to
## recompute said matrix multiple times. Let's look at an example below:

## Here we'll create a function called makeCacheMatrix which will store a list of functions
## for it to create a matrix and cache its results:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
    
  }
  get <- function() x
  
  setinvrs <- function (solve) invrs <<- solve
  
  getinvrs <- function () invrs
  
  list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
  

}


## Here we'll create a function called cacheSolve to store our makeCacheMatrix function
## and calculate the inverse of the matrix created in makeCacheMatrix. If the inverse of
## of this matrix has already been computed, then this function will simply retrieve it
## from the cache

cacheSolve <- function(x, ...) {
        ## this will return a matrix that is the inverse of 'x'
  invrs <- x$getinvrs()
  
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ... = )
  x$setinvrs(invrs)
  invrs
  
}


