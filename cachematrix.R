## The inverse of the given matrix will be cached 
## using two functions (makeCacheMatrix and cachesolve).

## makeCacheMatrix: generates a matrix capable of 
## caching the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

  ## Provides the property of the inverse
  i <- NULL
  
  ## Step 1: Set the matrix value
  set <- function(y) {
    x <<- y
    i <<- NULL  
}

  ## Step 2: Get the matrix value
  get <- function() x
        
  ## Step 3: Set the Inverse Matrix values
  setInverse <- function(inverse) i <<- inverse
        
  ## Step 4: Get the Inverse Matrix Value
  getInverse <-function() i
        
  ## Generate a list for the function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
        
## cacheSolve: Calculates the inverse of the cache matrix 
## that the first function created.
cacheSolve <- function(x, ...) {
        
  ## Return a matrix which is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Processing...")
    return(i)
  }
        
  ## Get the matrix from the object
  m <- x$get()
        
  ## Generate the inverse matrix
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
