getwd()
## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  
  # Setting default values for inv ('is inverted' flag)
  inv <- NULL
  
  # Declaring 'set' function for new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Declaring 'get' function to get current matrix
  get <- function() x
  
  # Declaring 'setinv' function to solve a matrix
  setinv <- function(solve) inv <<- solve
  
  # Declaring 'getinv' function to get state of inversion 
  getinv <- function() inv
  
  # Declaring a list to get access to functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Write a short comment describing this function


cacheSolve <- function(x = matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get flag is the matrix inversed and cached 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If inversion is not cached, get current matrix and solve it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

