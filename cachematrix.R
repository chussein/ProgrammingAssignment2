## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    ## assigns <<- so x and i can be pulled from outside of the current environment
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  
  ## list turns the functions into a list so that they can be used in the next function with the $ sign
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {  
    ## gets the cached data if it has been computed already
    message('getting cached data')
    return(i)
  }
  ## if it hasn't it runs this to compute the data
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i  
}
