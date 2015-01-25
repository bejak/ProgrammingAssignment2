## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##creates set function which takes y and sets it to x out of its environment then sets m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##to get the matrix 
  get <- function() x
  ## to set the solved matrix
  setmatrix <- function(solve) m <<- solve
  ##toget the solved matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		##makes m equal to the unsolved matrix
  m <- x$getmatrix()
  ## checks if m is not null this is checking matrix has been solved and cached
  if(!is.null(m)) {
    ##if it has been solved and cached then it reutrns m and a message
    message("getting cached data")
    return(m)
  }
  ##it solves the inverse of the matrix and saves it in the cache for the given matrix
  else{
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  }
}
