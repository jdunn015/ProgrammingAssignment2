## The below two functions create a special object that stores an invertible matrix and caches its inverse. 

## Upon call within cacheSolve, makeCacheMatrix stores the value of input matrix into "x"
## with the function "set" and initialies inverse matrix "m" to NULL.  The function "get"
## allows the parent function cacheSolve to retrieve the value of input matrix "x". 
## "Setinverse" sets "m" to the inverse of "x".  "getinverse" allows parent function
## cacheSolve to retrieve the cached inverse matrix "m". 

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


## cacheSolve calculates the inverse of the special "vector" created with the above function.
## It first checks to see if the inverse has already been calculated.  If so, it gets the inverse
## from the cache and skips the computation.  Otherwise, it calculates the inverse of the data
## and sets the value of hte inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
}


cacheSolve(makeCacheMatrix(testmatrix))

