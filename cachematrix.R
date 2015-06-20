
## This is to create a pair of functions that can cache the inverse of a matrix

## Here is a function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    
         x <<- y
         i <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Here is a function to compute the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
    
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}