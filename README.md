# assignment2_lexical_scoping


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  ## Return inverse of matrix 
  inv <- x$getInverse()
  if (!is.null(inv)) {
   
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


output

 my_matrix$get()
     [,1] [,2]
[1,]    4    7
[2,]    2    6

cacheSolve(my_matrix)
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4
