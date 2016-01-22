## Function required for the Programming Assignement 2 (R Programming on Coursera)

## makeCacheMatrix(X):
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(X=matrix()){
  inv_X <- NULL
  
  set <- function(Y) {
    X <<- Y
    inv_X <<- NULL
  }
  
  get<-function() X
  
  setinv <- function(inv_X2) {
    inv_X<<-inv_X2
    inv_X
  }
  
  getinv <- function() {
    inv_X
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)  
  
}

## cacheSolve(makeCacheSove(X)):
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix X has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## If the matrix X is changed respect the last run session the inverse of X i computed
## and the state of the corrisponding variable (inv_X)
## in makeCacheMatrix is upgraded with the new result.

cacheSolve <- function(l=makeCacheMatrix, ...) {
    inv_X <- l$getinv()
    
    if(!is.null(inv_X)) {
      message("X is NOT changed --> X^-1 is the same as before")
      X
      return(inv_X)
    }
    
    message("X is changed --> X^-1 is different respect the step before")
    newX<-l$get()
    inv_X2 <- solve(newX)
    l$setinv(inv_X2)
    
    print(inv_X2)
    return(inv_X2)
}
