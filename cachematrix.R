## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## It stores a list of functions i.e. set(), get(), setinverse() and getinverse()
makeCacheMatrix <- function(x = matrix()) {
      ## sets/initialise the inverse i to null
      i <- NULL
      ## set is a function that changes the matrix x stored in main function
      set <- function(y) {
            ## It subsitutes matrix x with y (the input) in the main function (makeCacheMatrix)
            x <<- y
            ## restores to null the value of the inverse i, because the old inverse of the old matrix is not needed anymore
            i <<- NULL
      }
      ## get is a function that returns the matrix x stored in the main function
      get <- function() x
      ## setinverse is a function that stores the value of inverse
      setinverse <- function(inverse) i <<- inverse
      ## getinverse is a function that returnes the value of inverse
      getinverse <- function() i
      ## To store all the four functions list is used
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      ## Check if the value of matrix inverse is present i.e. not null
      if(!is.null(i)) {
            ## prints the message on console
            message("getting cached data")
            ## Return a cache matrix that is the inverse of 'x'
            return(i)
      }
      ## Gets the matrix stored with makeCacheMatrix
      data <- x$get()
      ## Compute the inverse of the matrix
      i <- solve(data, ...)
      ## Stores it in the object generated assigned with makeCacheMatrix
      x$setinverse(i)
      ## Return the inverse
      i
}

# To test:
# Create a random matrix. Eg:-
# > a=matrix(data=c(1,2,3,4),nrow = 2, ncol = 2)
# or
# > a=rbind(c(1,2),c(3,4))
# > v <- makeCacheMatrix(a)
# > v$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# > cacheSolve(v)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# Run again to check if retrieves from cache
# > cacheSolve(v)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
