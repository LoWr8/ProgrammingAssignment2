## These two functions enable the efficient calculation of inverse matrices, as it enables cache storing 
## of already calculated inverse matrices and so prevents time insensitive recalculcation of inverse matrices.


## The function makeCacheMatrix creates an object, that enables the cache storing of a matrix and its
## corresponding inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) M <<- Inverse
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve uses an object created by the makeCacheMatrix for returning the inverse matrix 
## of the matrix stored in the object created by makeCacheMatrix. It first checks wether the inverse matrix
## has already been calculated. If it hasn't, it calculates the inverse matrix, stores it in the object and 
## returns it.

cacheSolve <- function(x, ...) {
  M<-  x$getinverse ()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
  
}
