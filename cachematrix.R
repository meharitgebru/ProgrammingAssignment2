## Put comments here that give an overall description of what your
## Programming Assignment 2 
## By M G, 10/9/2017
## Functions to store/cache an inverse of a matrix along with the matrix

## Function to set and get values of matrix and inverse  
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function to solve inverse if previously not calculated 
## Gets inverse if already exists 
## Returns inverse of a matrix, input makeCacheMatrix
cacheSolve <- function(x, ...) {
    
	i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
