## Functions for saving the inverse of matrix as cache and calling it from memory later when recalculated


## makeCacheMatrix accepts the the given matrix and calculate the inverse of it
## this inverse value is stored as cache for later calling

makeCacheMatrix <- function(x = matrix()){
                  m <- NULL
                  set <- function(y){
                              x <<- y
                              m<<- NULL
                  }
                  get <- function() x
                  setinverse <- function(inverse) m <<- inverse
                  getinverse <- function() m
                  list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
}


## cacheSolve function Returns the inverse of special matrix.
## If the inverse of matrix is already calculated in the above function then  
## returns the value saved in the cache

cacheSolve <- function(x, ...){
              m <- x$getinverse()
              if(!is.null(m)){
                        message("getting cached data")
                        return(m)
              }
              data <- x$get()
              m <- inverse(data, ...)
              x$setinverse(m)
              m
  
}