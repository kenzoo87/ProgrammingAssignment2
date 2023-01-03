## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS) # Library MASS is used to calculate inverse Matrix
##use the template provided by the exercise and the "mean" example
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() {invers <-ginv(x) #used to obtain the inverse matrix
                            invers%*%x
                            }
  list(set= set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
##use the template provided by the exercise and the "mean" example
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    m
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}

#test the function 
mdat <- makeCacheMatrix(matrix(1:10,2,5))
mdat$get()
mdat$getinverse()
cacheSolve(mdat)

mdat$setinverse()

#error
cacheSolve(mdat)
makeCacheMatrix(mdat)

