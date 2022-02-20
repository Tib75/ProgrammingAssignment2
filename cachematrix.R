## Those two functions are supposed to :
## makeCacheMatrix - Set / Get and create a solve cache Matrix
## cacheSolve - Test the cache and call solve / set solve if its empty

## makeCacheMatrix Function is used to get / set solve
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## cacheSolve is a function aimed at checking if the x parameters is filled or not
## and get the cached value if its filled or call the solve function and cache it if its empty
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
