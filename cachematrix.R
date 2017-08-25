## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix is really a list containing a functions:
## 	1. set - to set the value of the matrix
##	2. get - to get the value of the matrix
##	3. setsolve - set the value of the inverse
##	4. getsolve - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) i <<- solve
	getsolve <- function() i
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function written above:
##	cacheSolve first checks to see if the inverse has already been calculated.
##	If so, cacheSolve gets the inverse from the cache and skips the computation.
##	Otherwise, cacheSolve calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function. 

cacheSolve <- function(x, ...) {
	i <- x$getsolve()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setsolve(i)
	i  
	## Return a matrix that is the inverse of 'x'
}