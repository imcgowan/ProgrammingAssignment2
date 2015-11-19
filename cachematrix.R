## Below is a pair of functions that are used 
## to cache the inverse of a matrix.

## This funtion will create and return a list of functions used by
## the 'cacheSolve' function. It will get or set the matrix inversion
## in a cache.
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  ## create the matrix in the working env.
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## invert matrix and cache it
  sMatrix <- function(inverse) cache <<- inverse
  
  ## call the matrix from the cache
  gInverse <- function() cache
  
  ## send functions to the work env
  list(set = set, get = get, sMatrix = sMatrix, gInverse = gInverse)
  
} ##end function


##This function will calculate the inverse matrix from the 'makeCacheMatrix'
## function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## If cached, it will get the inverse of the matrix
  cache <- x$gInverse()
  
  ## return the inverted matrix from the cache
  if(!is.null(cache)){
    message("The data is cached")
    
    ## return the cache
    return(cache)
    
  } # end if
  
  ##if it's not there, create the matrix
  matrix <- x$get()
  
  ## set and return the inverse
  cache <- solve(matrix, ...)
  
  ## set the inverted matrix
  x$sMatrix(cache)
  
  ## return the cache
  return(cache)
  
} # end function
