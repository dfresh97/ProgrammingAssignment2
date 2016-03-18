## The two functions below can be used to cache the inverse of a matrix

## makeCacheMatrix creates a list to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
      x <<- y
      inv <<- NULL
}
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function calculates the inverse of the matrix created with the above function. 
## It checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
      message("getting cached data")
      return(inv) 
}
  mat.data = x$get()
  inv = solve(mat.data,...)
  x$setinv(inv)
  return(inv)
}
a <- diag(4,4)
a
## [,1] [,2] [,3] [,4]
## [1,]    4    0    0    0
## [2,]    0    4    0    0
## [3,]    0    0    4    0
## [4,]    0    0    0    4
CacheMatrix <- makeCacheMatrix (a)
cacheSolve(CacheMatrix)
## [,1] [,2] [,3] [,4]
## [1,] 0.25 0.00 0.00 0.00
## [2,] 0.00 0.25 0.00 0.00
## [3,] 0.00 0.00 0.25 0.00
## [4,] 0.00 0.00 0.00 0.25
cacheSolve(CacheMatrix)
getting cached data
##[,1] [,2] [,3] [,4]
##[1,] 0.25 0.00 0.00 0.00
## [2,] 0.00 0.25 0.00 0.00
## [3,] 0.00 0.00 0.25 0.00
## [4,] 0.00 0.00 0.00 0.25