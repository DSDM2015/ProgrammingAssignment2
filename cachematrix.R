## Functions to calculate and return the inverse of a matrix x
## To save computing time, a cached value is used if the computation
## has already been done earlier
## Uses superassignment operator (<--) to assign values to variables
## created in other environments than the current function

## Creates a list containing 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## If already cached, it will return the cached value (and print out a message accordingly)

cacheSolve <- function(x, ...) {
        
  m <- x["getinverse"]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x["get"] 
  m <- solve(x, ...)
  x["setinverse"]
  m
}
