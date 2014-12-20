#####################DESCRIPTION of CODE###############################
#makeCacheMatrix wraps a matrix function and defines four functions(set, get, setinverse, getinverse). cacheSolve takes a cachable matrix, computes its inverse and stores it in the cache. If cacheSolve is executed a second time, the cached matrix is returned. 

## makeCacheMatrix
#is a function with one parameter x (class:matrix)
#it returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #object to be cached
  set <- function(y) { #a function with one parameter 'y'
    x <<- y #assigns scoped variable x the value of y
    inverse <<- NULL #resets inverse to 'NULL' 
  }
  get <- function() x #returns the matrix
  setinverse <- function(result) {inverse <<- result} # assigns inverse the value of result
  getinverse <- function() inverse #a function that returns inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
#computes inverse matrix and caches it

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() #executes getinverse() to get the current value of 'inverse'
  if(!is.null(inverse)) {
    message("getting cached data") 
    return(inverse)
  }
  data <- x$get() #executes get();the not-inverted matrix 'x' is assigned to 'data'
  inverse <- solve(data, ...) #assigns inverse matrix to variable 'inverse'
  x$setinverse(inverse)
  inverse
}

##################TEST###########################
#

m <- matrix(4:1,2,2) #a test matrix
m
cachableMatrix <- makeCacheMatrix(m) # returns a list of functions
cacheSolve(cachableMatrix)
cacheSolve(cachableMatrix) # execute again to test if cacheSolve works --> test caching: does it return "getting cached data"