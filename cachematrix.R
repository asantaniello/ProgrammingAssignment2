## The functions in this file compute, cache, and return matrix inversions.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# It assumes that the matrix stored in makeCacheMatrix is square and invertible.
cacheSolve <- function(mcm) {
  # Return a matrix that is the inverse of 'mx'
  # Attempt to get from the makeCacheMatrix
  inv <- mcm$getinverse()
  if(!is.null(inv)) {
    # inversion has already been made - fetch from makeCacheMatrix list
    message("getting cached matrix inversion")
    return(inv)
  }
  # inversion not made yet. 
  # get matrix from makeCacheMatrix list, compute inversion, save back to makeCacheMatrix, and finally return inversion
  data <- mcm$get()
  inv <- solve(data)
  mcm$setinverse(inv)
  inv
}
