## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <-NULL
  
  set <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(mat)
  {
    inverseMatrix <<- mat
  }
  
  getinverse <- function() inverseMatrix
  
  list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getinverse()
  
  if (!is.null(iMatrix))
  {
    
    message("getting matrix from cache")
    return (iMatrix)
  }
  data <- x$get()
  iMatrix <- solve(data,...)
  x$setinverse(iMatrix)
  
  iMatrix
}
