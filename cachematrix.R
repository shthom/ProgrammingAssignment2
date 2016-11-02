
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  

## makeCacheMatrix exposes methods to get a matrix, set a matrix, get a cached version of the matrix and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## the variable to store the inverse of the matrix
  inverseMatrix <-NULL
  
  ## the matrix set method
  set <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
    
  }
  
  ## method to return the original matrix
  get <- function() x
  
  ## method for assigning the inverse of the matrix to the inverseMatrix variable
  setinverse <- function(mat)
  {
    inverseMatrix <<- mat
  }
  
  ## method to return the inverse of the matrix 
  getinverse <- function() inverseMatrix
  
  list(get = get,set = set,setinverse = setinverse,getinverse = getinverse)

}



## cacheSolve takes in a matrix and returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getinverse()
  
  if (!is.null(iMatrix))
  {
    ## get the inverse of the matrix from cache
    message("getting matrix from cache")
    return (iMatrix)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse
  iMatrix <- solve(data,...)
  
  x$setinverse(iMatrix)
  
  ## return the inverse
  iMatrix
}
