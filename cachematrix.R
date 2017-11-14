## Matrix inversion is usually a costly computation and there may be some  
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function(1) creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# var carry invers value
        i <- NULL
# matrix set function
        set <- function(y)
       {
        x <<- y
        i <<- NULL
       }
# matrix get function
  get <- function() x
# inverse get, set functions
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function(2) computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
