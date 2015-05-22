## Define two functions support you cache the inverse of given matrix


makeCacheMatrix <- function(x = matrix()) {
  # Create object to save matrix and it's inverse matrix
  #
  # Args:
  #   x: input matrix, default is empty matrix
  #
  # Returns:
  #   list contains input matrix and it's inverse matrix
  #
  
  cachedMatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(newMatrix) cachedMatrix <<- newMatrix
  getInverseMatrix <- function() cachedMatrix
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


cacheSolve <- function(x, ...) {
  # Calculate the inverse of matrix,
  # if input matrix is calculated, return the cached matrix
  # 
  # Args:
  #   x: object which input matrix is stored
  # 
  # Returns:
  #   the inverse matrix of given matrix
  #   
  
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
