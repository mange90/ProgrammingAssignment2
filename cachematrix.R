## Functions that stores a matric and its iverses

## Creates an object conatining a matric and it inverese

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_inv <- function() inv_mat <<- solve(x)
  get_inv <- function() inv_mat
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Gets the inverse matrix of a makeCacheMatrix object

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv()
  inv
}
