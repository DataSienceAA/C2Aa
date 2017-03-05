## Calculating an inverse matrix is usually a costly operation, and it can be beneficial
## to cach the inverse matrix rather than compute it several times. makeCacheMatrix and 
## cacheSolve functions are used to cache the inverse matrix
## For this assignment, we assume that the matrix supplied is always invertible.

## makeCacheMatrix creates a list containing the following functions:
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
      
      x <<- y
      
      inv <<- NULL
      
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
  }



## cacheSolve function returnes the inverse matrix.
## If the inverse matrix has already been calculated, it takes the result without additional computation.
## If the matrix has not been calculated, the function calculates the inverse matrix and sets the value
## in the cache using setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data.")
    
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv
}
