## Pair of functions to create a cacheable inverse matrix calculator

## Special matrix with cacheable inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             
  set <- function(y) {                    
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() x                     
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## This function will return inverse of the matrix. If the matrix inverse is already calcuated and chached, it will return the cached value without ##recomputation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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