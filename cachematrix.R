## These functions taken together allow finding inverses of matrices
## where the calculation is not repeated if it has already been made on the
## current matrix
## this implementation by Jeremy Kahan 4/26/2016

## set up a special matrix object with the ability to cache and retrieve the calculated inverse
## (might be fun to expand it to cover other intense calculations like determinants or row reduced echelon form)
makeCacheMatrix <- function(x = matrix()) {
	CachedInverse <- NULL #initially empty until calculated
	set <- function(y){
		x<<-y
		CachedInverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) CachedInverse <<- inverse
	getinverse <- function () CachedInverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns methods
}


## function to use an object set up with makeCacheMatrix to get the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#per the assignment guidelines, we assume the inverse exists
	RetVal <- x$getinverse()
	if(!is.null(RetVal)){
		message("getting cached data")
		return(RetVal)
	}
	#if we are still here the cache was empty so we calculate and cache
	MatrixToInvert <- x$get()
	RetVal <- solve(MatrixToInvert, ...)
	x$setinverse(RetVal)
	RetVal
}
