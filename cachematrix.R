## Assignment for Coursera Lesson week 3 (January 2015) 
##    Write the following functions:
##         makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##         cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Here is the first function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <-function(x = matrix())
{
  ## m : cache for the inversed matrix
  ## We initially set the cache to NULL to detect the first call
  m <-NULL
  
  ## set : be able to set a matrix in this object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get : be able to get matrix of this object
  get <- function() x
  
  ## setinv : be able to set the cached inversed matrix
  setinv <- function(invm) { 
    print("set cache data")
    m <<- invm                              
  }
  ## getinv : be able to get the cached inversed matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Here is the second function that computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <-function(x, ...)
{
  ## check existence of the cache
  m <- x$getinv()
  if(!is.null(m)) {
    ## yes we already have the cached
    message("getting cached data")
    return(m)
  }
  ## no cache
  data <- x$get()  # we get the matrix 
  m <- solve(data) # we solve the matrix
  x$setinv(m)      # we cache the inversed matrix for futur calls
  m                # we return the computed inversed matrix
}

