## The inverse of the given matrix will be cached using 
## two functions (makeCacheMatrix and cachesolve).

## makeCacheMatrix: generates a matrix capable of 
## caching the inverse matrix

makeCacheMatrix <- function(a = matrix()) {

  ## The inverse property is supported by this
  i <- NULL
  
  ## Step 1: Set the matrix value
  set <- function(b) {
    a <<- b
    i <<- NULL  
}

  ## Step 2: Get the matrix value
  get <- function() a
        
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
        
## cacheSolve : Calculates the inverse of the cache matrix
## which was created by the first function
cacheSolve <- function(a, ...) {
        
  ## Return a matrix that is the inverse of 'a'
  i <- a$getInverse()
  if (!is.null(i)) {
    message("Processing...")
    return(i)
  }
        
  ## Get the matrix from the object
  m <- a$get()
        
  ## Generate the inverse matrix
  i <- solve(m, ...)
  a$setInverse(i)
  i
}
