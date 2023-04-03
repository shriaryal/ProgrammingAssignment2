# Programming Assignment 2

#makeCacheMatrix function creates a matrix object that can cache it's inverse


makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL 
  set <- function(y){  #sets the value of a matrix
    x <<- y
    inversematrix <<- NULL
  }
  # gets the value of a matrix 
  get <- function() x
  #set the value of inverse of a matrix
  setinversematrix <- function(inverse) inversematrix <<- inverse 
  # get the value of inverse of a matrix
  getinversematrix <- function() inversematrix  
  list(set = set, get = get, setinversematrix = setinversematrix,getinversematrix = getinversematrix )

}


## cacheSolve function returns the inverse value of matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix = x$getinversematrix()
  
  if(!is.null(inversematrix)){
    message("getting cached inverse matrix")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinversematrix(inversematrix)
  return(inversematrix)
}
