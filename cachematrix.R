##Two function makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv
##library(MAss) is used to calculate inverse for non squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x        #function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInv<-function(){
                     inver<-ginv(x)
                     inver%*%x       #function to obtain inverse of the matrix
                     }
  list(set = set,get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data ...)
  x$setInv(inv)
  inv
}