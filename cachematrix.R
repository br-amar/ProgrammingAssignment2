## The below two functions are inter-dependent. The first function 
## creates a special matrix containing a flag to indicate if its already
## inversed. The second function would inverse this special matrix if its
## not already inversed.


## makeCacheMatrix function below will create a special matrix that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	getinv <- function() inv
	setinv <- function(inverse) inv <<- inverse
	getmatrix <- function() x
	
	list(getinv = getinv, 
			setinv = setinv, 
			getmatrix = getmatrix)

}


## This function would first check if the passed matrix is already inversed.
## If not, it will inverse the matrix.

cacheSolve <- function(x, ...) {
	i <- x$getinv()

	if (!is.null(i)) {
		message("Getting cached inverse matrix")
		return (i)
	}

	matrix <- x$getmatrix()
	i = solve(matrix)
	x$setinv(i)
	i
}
