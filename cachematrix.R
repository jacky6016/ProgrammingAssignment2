## Return a function and store data that will further be used be the function 'cacheSolve'
makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of input matrix to the function 'makeCacheMatrix'
cacheSolve <- function(x, ...) 
{
  m <- x$getInverse()
  # Data already cached
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  # Data not cached
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
