## Just like the example function makeVector, this function makeCacheMatrix, creates a set of functions and returns these
## with a link to the parent environment. Only this function works with a matrix instead of a vector.
## This function makes it possible for cacheSolve to calculate and store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                           ## initializing the objects, x and inv
  set <- function(y) {
    x <<- y                                             ## puts x in the parent environment
    inv <<- NULL                                        ## resets inv in the parent environment
  }
  get <- function() x                                   ## defining the getter for the matrix x
  setinverse <- function(solve) inv <<- solve           ## defining the setter for inv (which is the inverse of x)
  getinverse <- function() inv                          ## defining the getter for the matrix inv
  list(set = set, get = get,                            ## creating a list with all four set/get functions to be able -
       setinverse = setinverse,                         ## to call them with their name in the cacheSolve function
       getinverse = getinverse)
}

## The cacheSolve function takes an argument x (of the form makeCacheMatrix(x)) and computes and stores its inverse so that
## it can be called later on

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(inv)
  inv
}
