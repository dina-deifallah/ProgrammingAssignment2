## Below are two functions that are used to create a special object that 
## stores a numeric matrix and caches its inverse

## This function will create a "special" matrix, which is effectively a list
## containing 4 functions to:
## 1. get the value of the matrix
## 2. set the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function calculates the inverse of the special matrix x generated from the 
## previous function. It will use the cashed inverse if it exists (i.e. not equal
## to NULL), otherwise it will use the function solve to get the 
## inverse of the data matrix used as input in the previous function and will 
## cache the inverse using the setinverse() function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
