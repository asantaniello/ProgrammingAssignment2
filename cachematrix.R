# The functions in this file compute, cache, and return matrix inversions.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inversion matrix to NULL
  inv <- NULL
  # 'set' will hold a function to assign the user's input matrix to the scoped variable 'x'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 'get' will hold a function that just returns the user's input matrix
  get <- function() x
  # 'setinverse' holds a funcion that assigns the inputed inversion matrix to inv
  setinverse <- function(i) inv <<- i
  # 'getinverse' holds a function just returns the inverted matrix that was set with 'setinverse'
  getinverse <- function() inv
  # Assemble these functions into a list
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
  # Inversion not made yet. 
  # Get matrix from makeCacheMatrix list, compute inversion, save back to makeCacheMatrix, and finally return inversion
  data <- mcm$get()
  inv <- solve(data)
  mcm$setinverse(inv)
  inv
}
