## The objective is to return the inverse of a matrix if it is
## already calcuated for that matrix and stored in the cache, 
## otherwise calculate the inverse matrix and return.
## Assumption is that the input matrix is always invertible.
## The matrix whose inverse is to be returned by calling cacheSolve()
## must be of type class 'makeCacheMatrix' only

## Usage Details:
## > source("cachematrix.R")
## > a <- makeCacheMatrix(matrix(c(1, 6, 7, 4, 2, 4, 9, 2, 6), c(3, 3)))
## > cacheSolve(a)
##            [,1] [,2]      [,3]
## [1,]  0.6666667  2.0 -1.666667
## [2,] -3.6666667 -9.5  8.666667
## [3,]  1.6666667  4.0 -3.666667

## running again will print the message first as it is being read from cacahe.
## > cacheSolve(a)
## getting cached data
##            [,1] [,2]      [,3]
## [1,]  0.6666667  2.0 -1.666667
## [2,] -3.6666667 -9.5  8.666667
## [3,]  1.6666667  4.0 -3.666667



## This function creates a matrix which contains a list of below functions:
## 1. set() - sets the matrix values
## 2. get() - returns the matrix values
## 3. setinverse() - sets the inverse matrix values
## 4. getinverse() - returns the inverse matrix values
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y	
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inv1) {
		inv <<- inv1
	}
	getinverse <- function() {
		inv
	}
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse matrix if it is already available in
## the cache, otherwise calculates, stroes and returns it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
