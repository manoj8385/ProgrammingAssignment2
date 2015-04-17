## Cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) ## set function, use  to set matrix to object created by makeCachematrix function
  {
    x <<- y
    m <<- NULL
  }
  get<- function()x ## return the input matrix
  setsolve <- function(solve) m <<- solve ## Set the inversed matrix
  getsolve <- function()m ## Return the inversed matrix
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) ## It will be null if does not exists
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
