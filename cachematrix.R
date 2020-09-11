## 

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  ## setting value of matrix using another function
  ## not the usage of double arrow
  ## capable of modifying the parent function
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Implementing different methods of the function
  
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## to extract cache data

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data !")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}