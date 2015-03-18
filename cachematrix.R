# Matrix inversion is usually a computation intensive process.It saves resources
# to cache the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. In that case, it gets the result from the
# cache. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# The input matrix is assumed to be always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#Test
#> x <- rbind(c(7, 2, 1), c(0, 3, -1), c(-3, 4, -2))
#> testMatrix <- makeCacheMatrix(x)
#> testMatrix$get()
#      [,1] [,2] [,3]
#[1,]    7    2    1
#[2,]    0    3   -1
#[3,]   -3    4   -2
#> cacheSolve(testMatrix)
#      [,1] [,2] [,3]
#[1,]   -2    8   -5
#[2,]    3  -11    7
#[3,]    9  -34   21