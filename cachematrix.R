## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.


## MakeCAcheMAtrix creates a matrix 
## 1. "set" sets the values of the matrix
## 2. "get" gets the values of the matrix
## 3. "setinv" sets the the inverse matrix
## 4. "getinv" gets the inveres matrix 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <-function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,setinv = setinv, getinv = getinv)
}


## CacheSovle returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if (!is.null(inv)) {
          print("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
