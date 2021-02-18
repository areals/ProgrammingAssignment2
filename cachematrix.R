## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix sets value of the matrix and calculates its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {inv <<- inverse}
  getInv <- function() {inv}
  list(set = set, get = get, setInv = setInv, getInv = getInv)
       
}

## cacheSolve is used to get the cache data

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInv(inv)
    inv
}


