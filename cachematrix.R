
## The makeCacheMatrix function creates a special "matrix object" that can cache its inverse
# 1 set the matrix
# 2 get the matrix
# 3 set the inverse of the matrix by using the solve function
# 4 get the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
          m<-NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}

## The cacheSolve function retrieves the inverse of from the cache function above,
## or computes the inverse of the special matrix returned by makeCacheMatrix
# first it checkes whether the matrix is available from the makeCache matrix
# and adds a message that this comes from cached data
# else it is calculated again with the solve function 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

