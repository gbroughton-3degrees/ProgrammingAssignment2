# Programming assignment 2
# 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#             If the inverse has already been calculated (and the matrix has not changed), then the method retrieves the inverse from the cache.
# 
# To test, I typed the following into the console:
# 
# > mycached <- makeCacheMatrix(matrix(rexp(9), 3)) # generate a random 3 x 3 matrix 
# > mycached$get() # view the matrix
# > cacheSolve(mycached) # view the inverse matrix
# > cacheSolve(mycached) # should see 'returning cached version' message
# > mycached <- makeCacheMatrix(matrix(rexp(9), 3)) # generate another random 3 x 3 matrix 
# > cacheSolve(mycached) # matrix has changed, so you should NOT see the 'returning cached version' message

makeCacheMatrix <- function(x = matrix()) {
  
  cachedMatrix <- x 
  cachedInverse <- NULL
  
  get <- function() {
    return(cachedMatrix)
  }
  
  set <- function(matrix_in) {
    cachedMatrix <<- matrix_in
    cachedInverse <<- NULL
  }
  
  getinverse <- function() {
    return(cachedInverse)
  }
  
  setinverse <- function(matrix_in) {
    cachedInverse <<- matrix_in 
  }
  
  # define a list of functions that are exposed
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(cacheObj, ...) {
  ## Check to see if the object has already been cached
  inverseMatrix <- cacheObj$getinverse()
  if(!is.null(inverseMatrix)) {
    # already cached, so simply return value
    message("returning cached version")
    return(inverseMatrix)
  } else {
    inverse <- solve(cacheObj$get(), ...)  # calculate inverse
    cacheObj$setinverse(inverse)           # cache value
    return(inverse)                        # return value
  }
}
