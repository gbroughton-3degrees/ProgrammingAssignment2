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

# makeCacheMatrix accepts a matrix for caching
makeCacheMatrix <- function(x = matrix()) {
  
  cachedMatrix <- x 
  cachedInverse <- NULL
  
  # get a copy of the original matrix
  get <- function() {
    return(cachedMatrix)
  }
  
  # cache a copy of the original matrix
  # set the cached copy of the inverse matrix to NULL so we know that the original matrix has changed
  set <- function(matrix_in) {
    cachedMatrix <<- matrix_in
    cachedInverse <<- NULL
  }
  
  # get a copy of the cached inverse matrix
  getinverse <- function() {
    return(cachedInverse)
  }
  
  # cache a copy of the inverse matrix
  setinverse <- function(matrix_in) {
    cachedInverse <<- matrix_in 
  }
  
  # define a list of functions that are exposed
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve calculates the inverse of a matrix.
# If the inverse has already been calculated, it returns a cached copy
#
# cacheObj = object of type makeCacheMatrix
#
cacheSolve <- function(cacheObj, ...) {
  ## Check to see if the object has already been cached
  inverseMatrix <- cacheObj$getinverse()
  if(!is.null(inverseMatrix)) {
    # already cached, so show message and return value
    message("returning cached version")
    return(inverseMatrix)
  } else {
    inverse <- solve(cacheObj$get(), ...)  # calculate inverse
    cacheObj$setinverse(inverse)           # cache value
    return(inverse)                        # return value
  }
}
