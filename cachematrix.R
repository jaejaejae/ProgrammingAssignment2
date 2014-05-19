## create a matrix data structure which allow cache
makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverseM <<- inverseMatrix
  getInverse <- function() inverseM
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## solving inverse matrix
cacheSolve <- function(x, ...) {
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) {
    message("getting cache data")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setInverse(inverseM)
  inverseM
}
