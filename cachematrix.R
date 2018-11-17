## Making the matrix 
## Initializing with the objects X and Inv that have to be matrix.
## Then bellow I define the functions of the objects in the new makeCache
## matrix based on the function solve. 
## Then deliver it as a list. 

makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve)  inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## As it is made, running the solve function
## Function defined, then it will pull out the object previously
## created. 
## Then we set the inf condition to pull the cached object if
## it is available. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data :)")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Matrix to test - matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
