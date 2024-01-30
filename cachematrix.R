## Function to create a special matrix object with caching
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix and cache
  mat <- x
  inv_mat <- NULL
  
  # Function to set the matrix value
  set <- function(matrix) {
    mat <<- matrix
    inv_mat <<- NULL  # Reset the cache when matrix is set
  }
  
  # Function to get the matrix value
  get <- function() mat
  
  # Function to set the inverse matrix in the cache
  setInverse <- function(inverse) {
    inv_mat <<- inverse
  }
  
  # Function to get the inverse matrix
  getInverse <- function() inv_mat
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute the inverse of the matrix with caching
cacheSolve <- function(x, ...) {
  # Check if the inverse matrix is already in the cache
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # If not in the cache, compute the inverse using solve function
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Set the computed inverse in the cache
  x$setInverse(inverse)
  
  # Return the inverse matrix
  inverse
}
