##  This code contains a pair of functions that cache the inverse of a matrix.


#The makeCacheMatrix function creates a matrix that contains 4 sub-functions that performs
#the ff:
##set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  
  #set the inverse of the matrix to the cache
  setinverse <- function(solve) inv <<- solve
  
  #get the inverse of the matrix 
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
    
}


## The cacheSolve function computes the inverse of the "matrix".
##If the inverse was previously calculated , then the cachesolve retrieves the inverse 
#from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    #it was previously calculated, so retrieve the inverse from cache
    message("getting cached data")
    return(inv)
  }
  #gets the value of the matrix 'x' for computation
  data <- x$get()
  # computes the inverse of 'x' by looping
  inv <-lapply(data,solve)
  # Return a matrix that is the inverse of 'x'
  x$setinverse(inv)
  inv
  
 
}
