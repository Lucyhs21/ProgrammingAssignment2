## The below functions can be used to cache the inverse of a matrix rather than computing it repeatedly

## This function below creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
			Inverse <- NULL
			set <- function(y) {
				x <<- y
				Inverse <<- NULL
			}
			get <- function() x
			setInverse <- function(inverse)  Inverse <<- inverse
			getInverse <- function() Inverse
			list(set = set, get = get,
				setInverse = setInverse,
				getInverse = getInverse)
		}

## This function below then computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        MatrixInverse <- x$getInverse()
        if(!is.null(MatrixInverse)) {
        		message("getting cached data")
        		return(MatrixInverse)
        }
        data <- x$get()
        MatrixInverse <- solve(data, ...)
        x$setInverse(MatrixInverse)
        MatrixInverse
}
