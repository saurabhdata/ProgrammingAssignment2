
## Below Functions calculates the Inverse of Matrix and returns the Inverse from Cache if Calculated
## makeCacheMatrix: This function creates a special matrix and cache its inverse.
## cacheSolve: Returns the inverse of Matrix if already calculated, else calculate the inverse and return

## Creates a Matrix and Calculates its Inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Returns the inverse of Matrix if already calculated, else calculate the inverse and return
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
