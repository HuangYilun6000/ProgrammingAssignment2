## Below shows a pair of fucntions programmed to create an object to cache the 
## inverse of a matrix, thus saving time if it has to be computed repeatedly.

## The first function, makeCacheMatrix creates a special "matrix" that caches 
## its inverse, which is actually a list containing a function to both set and 
## get the value of the matrix, and both set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse <- function(inverse) m<<-inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function calculates the inverse of the special matrix created by 
## the first function. It can make sense to cache the value of the inverse and 
## directly retrieve it from the cache if the contents of a matrix are not 
## changing when we need it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  matr <- x$get()
  m <- solve(matr,...)
  x$setinverse(m)
  m
}
