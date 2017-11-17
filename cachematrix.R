## Put comments here that give an overall description of what your
## functions do
# testing
# i_mat<-matrix(c(2,3,4,5),2,2)
# invvector<-makeCacheMatrix(i_mat)
# invvector$set(i_mat)
# invvector$get()
## This function sets and gets the reverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function reverses and caches the matrix
# testing
# i_mat<-matrix(c(2,3,4,5),2,2)
# invvector<-makeCacheMatrix(i_mat)
# invvector$set(i_mat)
# invvector$get()
# invvector$setinverse(i_mat)
# invvector$getinverse()
# cacheSolve(invvector)

cacheSolve <- function(x, ...) {
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
