## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix generates and returns a list of functions so that cacheSolve can use to get the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
	# initial value 
	cache<-NULL
	#create a matrix in the working environment
	set<-function(y){
		x<<-y
		cache<<-NULL
	}
#get the matrix
get<-function() x
#invert the matrix and store it in cache
setMatrix<-function(inverse) cache<<-inverse
#get the inverse matrix from the cache
getInverse<-function() cache

#return the functions to working environment
list(set=set, get=get, setMatrix=setMatrix, getInverse=getInverse)
}


## This function calculates the inverse of the matrix generated in makeCacheMatrix. 

cacheSolve <- function(x, ...) {
	#attempt to get the inverse of the matrix stored in cache
	cache<-x$getInverse()
	#return the inverted matrix from cache
	if (!is.null(cache)){
		message("getting cached matrix")
		return(cache)
	}
	#generate the (assuming) invertible matrix
	matrix<-x$get()
	#calculate the inverse
	cache<-solve(matrix, ...)
	#set inverted matrix in cache
	x$setMatrix(cache)
	#display
	cache
}
