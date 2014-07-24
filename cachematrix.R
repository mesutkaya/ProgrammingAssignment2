## Functions below are for caching an inverse of a matrix. Sometimes matrix inversion calculation can be costy. İnstead of calculating 
## the inverse of a function each time, by caching it the inverse will be calculalated only once. 
## The code that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse is calculated before it returns the cached matrix instead of calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
