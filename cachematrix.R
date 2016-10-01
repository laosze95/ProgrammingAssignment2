## This function creates a special "matrix" object that can cache its inverse.
## The function output is a list of function which create the matrix and cache the inverse

## Creating and Caching the inverse of a special matrix.

makeCacheMatrix <- function(x = matrix()) {
		vInv <- NULL

		set <- function(y) {
		## the input y is a matrix
			x <<- y
			vInv <<- NULL
		}

		get <- function() { x }
		setinv <- function(solve) { vInv <<- solve }
		## the input matrix for solve is the same matrix as in the set

		getinv <- function() { vInv }
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## This function retrieve the cached data for the inverse of the matrix, if present
## else, it will calculate the inverse and then cache the output for the further use
## The input for this function is the list vector created in the earlier function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	vInv <- x$getinv()

	if(!is.null(vInv)) {
	message("Returing Cached Data for the Inverse")
	return(vInv)
	}

	data <- x$get()
	vInv <- solve(data)
	x$setinv(vInv)
	## putting the new value of inverse into cache
	vInv
}
