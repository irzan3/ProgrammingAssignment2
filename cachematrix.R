## A pair of functions that cache the inverse of a matrix

## Function to create a special "matrix" object that can cache its inverse
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


## Function to compute the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
cachemean <- function(x, ...) {
  
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