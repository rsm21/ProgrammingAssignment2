
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inversefor the input
##whichis an invertible square matrix
makeCacheMatrix <- function(x = matrix()) {
j <- NULL
set<-function(y){
x<<-y
j<<-NULL
}
get<-function()x
setInverse<-function(inverse) j<<-inverse
getInverse<-function() j
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Write a short comment describing this function
##cacheSolve is a functionwhich computes the inverse of a special "matrix" returned by makeCacheMatrix above.
##If the inverse has been calculated (and matrix has not been changed )then the cacheSolve should retrieve the inversefrom the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		j<-x$getInverse()
		if(!is.null(j)){
		message("getting cached data")
		return(j)
		}
		mat<-x$get()
		j<-solve(mat,...)
		x$setInverse(j)
		j
}
