#makeCacheMatrix inputs a square matrix and returns a list of four function objects
#makeCacheMatrix inputs the list created by makeCacheMatrix and returns the inverse of the matrix

## Returns a list with four function objects for a given matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x<<-y
		inv <<- NULL
	}
	get <- function() x
	getinv <- function() inv
	setinv <- function(inverse) inv <<- inverse
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Returns the Inverse matrix from the list created by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
	if(!is.null(m)){
		print('Getting cached copy for the Inverse')
		return(m)
	}
	
	data <- x$get()
	m <- solve(data)
	x$setinv(m)
	m

}
