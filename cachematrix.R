
## These functions cache the inverse of a matrix( assuming given matrix is invertible)
## Inverse of the matrix is retrieved from cache if the matrix is not changed,instead
## of computing inverse every time.


## makeCacheMatrix generates a special 'matrix' object that can cache its inverse
## In order of cacheSolve function to compute inverse, it needs makeCacheMatrix type 
## object as input. For every new matrix makeCacheMatrix needs to be calculated first
## and output is given to cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set matrix and inverse to NUll
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # set inverse to i
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #get inverse of matrix
  getInverse <- function() i
  
  # Returns named list of  above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  
}



##  This function takes in makeCacheMatrix type object and computes the inverse.
## If the inverse is already computed i.e (if the given matrix has not changed)
## then, it retrieves inverse from cache rather than it computing again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  # If Inverse i is already computed, get inverse from cache
  if(!is.null(i)) {
    message('getting inverse from cached data')
    return(i)
  }
  
  # If there is no inverse in cache, then get data
  data <- x$get()
  
  # Calculate matrix inverse
  i <- solve(data, ...)
  
  # setting inverse to store in cache
  x$setInverse(i)
  
  # returns inverse
  i
  
}
