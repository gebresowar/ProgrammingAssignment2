# makeCacheMatrix() creates a special "matrix" object
# that can cache its inverse. It will set the the value of the matrix, get the value
# of the matrix, set the value of the matrix inversion, and get the inverse value. 
# Finally it produces a list of these operations and the storage environment for their values

# cacheSolve() computes the inverse of the special  "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

# The biggest 'bug' I ran into was the error "$ operator is invalid for atomic vectors" when running 
# the cacheSolve() function. 
# My working solution to this is to assign the an object to the output of makeCacheMatrix() and 
# then to run cacheSolve() on this object. For example:

# > test <- (matrix(rnorm(1:9),3,3))
# > t1 <- makeCacheMatrix(test)
# > cacheSolve(t1)

# This approach functions as expected. If you rerun the last command again you get the expected
# "getting cached data" message. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

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

 