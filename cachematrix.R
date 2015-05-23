## The following functions can be used together to 
## calculate the inverse of a matrix efficiently - caching to save 
## computation efforts


## This functions stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() {
            x
      }
      setinv <- function(solve) {
            m <<- solve
      }
      getinv <- function() {
            m
      }
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of a matrix.
## If it finds that the inverse has already been calculated,
## it retrieves the inverse from the cache instead of calculating it.
cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
