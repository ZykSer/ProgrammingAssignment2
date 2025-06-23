## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # variable to store the inverse matrix
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the cache when the matrix is replaced
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set the inverse matrix in the cache
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse matrix from the cache
  getinverse <- function() inv
  
  # Return a list of the four functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  # try to get the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")  # if already cached
    return(inv)
  }
  data <- x$get()            # get the original matrix
  inv <- solve(data, ...)    # compute the inverse
  x$setinverse(inv)          # store the inverse in the cache
  inv
}
