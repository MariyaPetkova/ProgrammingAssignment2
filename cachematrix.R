##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatr <- NULL                       #initialize the value of the Inversed Matrix
  set <- function(y) {                  #define set function to assign value to the Matrix in the parent environment
    x <<- y
    invMatr <<- NULL
  }
  
  get <- function() x                   #define get function to return the Matrix
  setinverse <- function (inverse) invMatr <<- inverse() #assign value of invMatr in parent environment
  getinverse <- function() invMatr
  list(set = set,                       #set the value of the Matrix
       get = get,                       #get the value of the Matrix
       setinverse = setinverse,         #set the value of the Inversed Matrix
       getinverse = getinverse)         #get the value of the Inversed Matrix
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invMatr <- x$getinverse()
  if(!is.null(invMatr)){
    message("getting cached data")
    return(invMatr)
  }
  data <- x$get()
  invMatr <- solve(data,...)
  x$setinverse(invMatr)
  invMatr
}
