## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


  cacheMatrix <- NULL

  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }

  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix

  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)



}


cacheSolve <- function(x, ...) {




  cacheMatrix <- x$getCache()

  #if the content is not null, return the result

  if (!is.null(cacheMatrix)) {
    return(cacheMatrix)
  }

  # if the content is empty,
  # get the matrix, create, set, update and return the cache matrix
  else {
    newMatrix <- x$getMatrix()
    cacheMatrix <- solve(newMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }

}
