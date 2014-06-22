# following two functions make it easy to cache the calculating the inverse of 
# of a matix.
# unit test
# a <- c(c(0,0,0,0,1),c(1,0,0,0,1),c(0,1,0,1,0),c(0,0,1,0,0),c(0,0,0,1,0))
# dim(a) <- c(5,5)
# b <- makeCacheMatrix(a)
# c <- cacheSolve(b)
# a %*% c

# Input: a matrix
# Output: a object that has the input matrix and functions, which allows
# cacheSolve() funciton to save the inverse of the matix to the object.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Input: an object obtained from function makeCacheMatrix()
#  output: the inverse of the matrix of the input. If the input 
#          object does't has its inverse, save the inverse to
#          the input object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    print("cached data")
    return(m)
  }
  print("calculated data")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

setTestData <- function() {
  a <- c(c(0,0,0,0,1),c(1,0,0,0,1),c(0,1,0,1,0),c(0,0,1,0,0),c(0,0,0,1,0))
  dim(a) <- c(5,5)
}





