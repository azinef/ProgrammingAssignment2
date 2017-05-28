## initialize empty matrices
## creates the cache which is a list of functions, set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function()m
  ##this list defines the output of the function of makecachematrix
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
## cachesolve returns inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## if the inverse has been calculated, get it from the cache and skips the computation.
  if (!is.null(m)) {
    message("cached data")
    return(m)
  }
  ## if not, calculates the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
