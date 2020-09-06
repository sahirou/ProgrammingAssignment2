
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions cache the inverse of a matrix, and then 
## recall it later for the same input matrix, instead of recompute it


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize th inverse of the x matrix to null
  inverse <- NULL
  
  # Define function to cache a new matrix value 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Define function to get cached matrix value
  get <- function() {
    return(x)
  } 
  
  # Define function to cache the inverse of the matrix
  setinverse <- function(inverse_mat) {
    inverse <<- inverse_mat 
  }
  
  # Define function to get cached inverse of the matrix
  getinverse <- function() {
    return(inverse)
  }
  
  # Return special "matrix"
  return(
    list(
      set = set, 
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
    )
  )

}



## This function check if the inverse of a a matrix is already computed and 
## cached in the special matrix from makeCacheMatrix function, returns 
## the cached value if it exists, else compute the inverse, store it in 
## the special matrix and return it 
cacheSolve <- function(x, ...) {
  
  # Get the inverse of the matrix cached by makeCacheMatrix
  inverse <- x$getinverse()
  
  # Check if the inverse has already been calculated and return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  } else {
    data <- x$get()
    # Calculate the inverse
    inverse <- solve(data, ...)
    # Cache the calculated inverse
    x$setinverse(inverse)
    # Return the calculate inverse
    return(inverse)
  }
  
}




