## These two functions can be used to store a matrix that may require its
## inverse to also be used. The matrix is cached in the current environment
## to help improve performance of matrix operations.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	myInverse = NULL
	set = function(input) {
		x <<- input
		myInverse <<- NULL
	}
	get = function() x

	## Use the parent environment assignment to store in cache
	setinv = function(inputInverse) myInverse <<- inputInverse

	getinv = function() myInverse
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Attempt to look up matrix in cache
	myInverse = x$getinv()
	if(!is.null(myInverse)) {
		return(myInverse)
	}

	## Not found in cache, compute the inverse, store in cache
	## and then return inverse.
	myMatrix.data = x$get()
	myInverse = solve(myMatrix.data, ...)
	x$setinv(myInverse)
	return(myInverse)
}

