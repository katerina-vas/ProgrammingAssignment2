#The makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#The cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
  m <- x$getinverse()   # retrieves & assigns old cache value
  if(!is.null(m)) {     # if the old cache value exist
    message("getting cached data") #then this message is printed
    return(m)  # the old cached value is printed
  }
  data <- x$get() #defines data to be the input data that user set
  m <- solve(data, ...) #defines m to be the inverse of data
  x$setinverse(m)      
  m
        ## Return a matrix that is the inverse of 'x'
}
