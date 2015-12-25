## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix solves the inverse of matrix x
# and saves it to m, then returns a list of functions;
# set,get,setInverse and getInverse
makeCacheMatrix <- function(x = matrix()) {

	# set the inverse matrix m NULL
	m <- NULL

	# function to replace matrix x with a new matrix y and keep it
	set <- function(y){
	x <<- y # keep x
	m <-NULL # set m NULL
	}
	
	# function to return the current matrix x
	get <- function() x
	
	# function to replace m with the formal
	setInverse <- function(inverse) m <<- inverse

	# function to return the inverse matrix m
	getInverse <- function() m

	# return a list with these 4 functions
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



# cacheSolve solves the inverse of matrix x 
# which was a formal for makeCacheMatrix()
# and saves it to maekCacheMatrix's environment.
# if it's called again, it retrievs the saved inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	# x is a list with 4 functions created with makeCacheMatrix
	# get the inverse from the environment when x was created 
	m <- x$getInverse()
	
	# if inverse is Not null, it was already solved and saved
	# use that inverse
	if(!is.null(m)) {
		message("getting cached inverse")
		return(m)
	}
	# if m is null, then compute inverse because this is the first time
	# get the matrix
	data <- x$get()
	# compute inverse
	m <- solve(data)
	#call x's setInverse so m can be saved in the environment when x was created
	x$setInverse(m)
	#return the inverse
	m
}

