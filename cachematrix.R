## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Note: The matrix supplied must be invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the cached objects
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  #return the matrix
  get <- function() x
  
  #create container functions for the inverse matrix
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  
  #store the four functions
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix) 
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix()
  
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  amatrix <- x$get()
  m <- solve(amatrix, ...)
  x$setinvmatrix(m)
  m #return the inverse matrix
}
