## Calculate the inverse of a matrix and cache it so it 
## only needs to calculate once

## make a CacheMatrix object to cache an invertable matrix

makeCacheMatrix <- function(x = matrix()) {
  cacheInv <- NULL
  
  set <- function(y){
    x <<- y
    cacheInv <<- NULL
  }
 
  get <- function() { x }
  setInverse <- function(inverse) {
    cacheInv <<- inverse
  }

  getInverse <- function() {
    cacheInv
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## return inverse of a CacheMatrix object, if the inverse has
## been calculated, the cached value will be returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
 
  inv <- solve(data, ...)

  x$setInverse(inv)
  inv      
}
