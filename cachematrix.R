## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function is to create a list of functions
# which set, get, setinverse and getinverse the give matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comments describing this function
# this function is to retrieve the inverse matrix of the given matrix
# if the inverse one has been computed, it will not compute again but return the computed one
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  # assign inverse matrix to i
  i <- solve(data)
  x$setinverse(i)
  i
}