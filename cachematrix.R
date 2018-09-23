## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
  
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


# To test the matrix
testMatrix <- cbind(1:2, 3:4)
testMatrix
aMatrix <- makeCacheMatrix(testMatrix)
aMatrix$get()
cacheSolve(aMatrix) 
aMatrix$set(cbind(10:11, 30:31))          # reset value with a new matrix
cacheSolve(aMatrix)          # inverse calculated for new matrix
aMatrix$getInverseMatrix()  

