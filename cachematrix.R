## `makeCacheMatrix` function creates a special list using "matrix" object that can cache its inverse.
## `cacheSolve` function computes the inverse from the special list returned by `makeCacheMatrix` above
##  If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## The following function creates a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse of the matrix, get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse from the list created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it `get`s the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
          print('Getting inverse from cache')
          return(inv)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
