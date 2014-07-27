#This function creates a matrix in the parent environment and methods to get the inverted matrix of a matrix created in that environment
makeCacheMatrix <- function(mmatrix = numeric()) {
	#Make sure there isn't an object with the same name as my matrix in the local environment
	cachematrix <- NULL
	#Create a sub-function called by this function to initialise the new matrix data and functions in the parent environment
	set <- function(newmatrix) {
		mmatrix <<- newmatrix
		cachematrix <<- NULL
	}
	#Create a function to allow the cached data to be retrieved
	get <- function() mmatrix
	#Create functions to allow the matrix to be solved and returned to the calling environment
	setinversion <- function(solve) cachematrix <<- solve
	getinversion <- function() cachematrix
	list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}

#This function allows the user to interact with the cached version of the matrix
cacheSolve <- function(mtrx) {
	#Get the inverted matrix from the cached inversion function
	cachematrix<- mtrx$getinversion()
	#Check if it has been calculated
	if( !is.null(cachematrix) ) {
		print("Getting cached version of the matrix")
		return(cachematrix)
	}
	#Get the data from the parent environment
	envmatrix <- mtrx$get()
	cachematrix<- matrix(envmatrix)
	#call the inversion function on it
	mtrx$setinversion(cachematrix)
	#and display it
	cachematrix
}

