## makeCacheMatrix - Create an augmented matrix object
##         that contains functions to store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the inverse  
  i <- NULL
  ##set function to intialize augmented matrix object  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ##get function to retrieve augment matrix object
  get <- function() x

  ##setinverse function to set inverse of the matrix  
  setinverse <- function(inverse) i <<- inverse

  ##setinverse function to get the inverse of the matrix
  getinverse <- function() i

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ##if inverse exists and matrix has not changed
  ## If matrix has not changed, matrix product should give identity matrix

  data <- x$get()
  i <- x$getinverse()
  if (!is.null(i)  && !identical(data,i)) {
    message("getting cached data")
    return(i)

  }

  ## Compute inverse, set the cache and return the inverse
  i <- solve(data)
  x$setinverse(i)
  return(i)
}

x <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
m <- makeCacheMatrix(x)

# original matrix
m$get()

# first call is uncached
cacheSolve(m)

# second call is cached
cacheSolve(m)
