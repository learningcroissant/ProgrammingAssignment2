## Put comments here that give an overall description of what your
## functions do


##This function creates a special "matrix" object that can cache its inverse.
##x is a square invertible matrix
##returns a list of functions that manipulate the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setMatrix <- function(y){
    x <<- y
    inverse <- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse #using double brackets because 'inverse' is in a diff. environment
  getInverse <- function() inv #simply returns the inverse matrix 
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## x is the output of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  #if the inverse has been calculated
  if(!is.null(inverse)){
    #get it from the cache and return it
    return(inverse)
  }
  #otherwise, calculate it
  
  calcInv <- x$get() #get the matrix
  inverse = solve(calcInv, ...) #solve is a generic function that takes a vector or matrix
  x$setInverse(calcInv) #setting the inverse to the cache
  return(inverse) #returning the calculated value
}
