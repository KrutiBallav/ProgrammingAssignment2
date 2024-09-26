## This set of functions allows for caching the inverse of a matrix, 
## which can save computation time if the inverse is needed multiple times.
## The 'makeCacheMatrix' function creates a special "matrix" object that
## can cache its inverse, while the 'cacheSolve' function computes or 
## retrieves the cached inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # 'inv' will store the cached inverse of the matrix. It is initialized as NULL.
  inv <- NULL                                   
  # The 'set' function assigns a new matrix to 'x' and resets 'inv' to NULL.
  # This is necessary because changing the matrix invalidates any previously cached inverse.
  set <- function(y) {                          
    x <<- y                                      # Assign new matrix value to 'x' in the parent environment.
    inv <<- NULL                                 # Reset 'inv' to NULL because the matrix has changed.
  }
  get <- function()x                             # The 'get' function returns the current matrix stored in 'x'.
  setinverse <- function(inverse)inv <<- inverse # The 'setinverse' function stores the calculated inverse in 'inv'.
  getinverse <- function()inv                    # The 'getinverse' function retrieves the cached inverse from 'inv'.
  # Return a list of the above four functions, allowing them to be accessed using the '$' operator.
  list (set=set, get=get, setinverse=setinverse,getinverse=getinverse)
 }

## This function computes the inverse of the special "matrix" created by 'makeCacheMatrix'.
## If the inverse has already been calculated (and the matrix has not changed),
## then 'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse, if it exists.
  inv <- x$getinverse()
  # If the cached inverse is not NULL, it means we have a previously computed value.
  # We return the cached value and skip re-computation.
  if(!is.null(inv)){
    message ("getting cached data")            # Inform the user that cached data is being used.
    return(inv)                                # Return the cached inverse.
  }
  # If the cached inverse is NULL, then we need to compute the inverse.
  # First, get the current matrix stored in 'x'.
  data <-x$get()
  # Compute the inverse using the 'solve' function in R.
  inv <- solve(data,...)
  # Store the computed inverse in the cache using the 'setinverse' function.
  x$setinverse(inv)
  # Return the newly computed inverse.
  inv
}
