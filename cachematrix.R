## makeCacheMatrix is a function which creates a special "matrix", that
## can cache the inverse for an input matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL           # s is our local variable
  set <- function(y){
    x <<- y # assigning value to object 'x', which is in different environment. Hence <<-. 
    s <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Now, cacheSolve is a function that finds the inverse of a special "matrix"
## which is returned by makeCacheMatrix. If the inverse is already been calculated 
## then it would take the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  my_matrix <- x$get()
  s <- solve(my_matrix,...)
  x$setInverse(s)
  s
  }
