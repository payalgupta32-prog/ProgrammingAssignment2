

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize cached inverse as NULL
  
  # Set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getinverse <- function() inv
  
  # Return a list of all the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setinverse(inv)
  
  # Return the inverse matrix
  inv
}
