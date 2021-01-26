## This r code consists of a pair of function that caches the inverse of a matrix

## This function sets the matrix and caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                    ## Intialize the inverse matrix
  setter<-function(y){         ## This function is used to set the matrix
    x<<-y
    inv<<-NULL
  }
  gets <- function() x         ## Methos to set the value of materix
  setinverse <- function(solve) inv <<- solve   ## Method to set the inverse of the matrix
  getinverse <- function() inv                  ## Method to get the inverse of matrix
        list(setter = setter, gets = gets,setinverse = setinverse,      ## List of the various methods
             getinverse = getinverse)

}


## This function returns the inverse of the matrix set in makeCacheMatrix.
## If a inverse is already calculated and the matrix has not changed, then
## cacheSolve returns the cache of the inverse matrix already computed.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()         ## Returns the inverse of matrix
      if(!is.null(inv)) {        ## Returns the cache of inverse matrix if already set
              message("getting cached data")
              return(inv)
      }
      data <- x$gets()          ## Gets the Intializeed new matrix
      inv <- solve(data, ...)   ## Computes the inverse  of new matrix
      x$setinverse(inv)         ## Sets the inverse of new matrix
      inv                       
        ## Return a matrix that is the inverse of 'x'
}
