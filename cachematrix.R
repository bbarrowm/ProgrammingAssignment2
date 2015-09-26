## calculates and caches the inverse of a matrix;
##retrieves inverse from cache if already calculated

## creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function (y) { 
    x <<- y
    i <<- NULL
  } ##sub-function 1: changes matrix stored in master function
  
  get <- function () x ##sub-function 2: returns the value of original function (requires no input)
  setinverse <- function(solve) i <<-solve  ##sub-function 3: sets the inverse of a matrix
  getinverse <- function () i ##sub-function 4: returns the inverse of matrix set by setinverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse) ##creates list of functions to store in makeCacheMatrix
    
}


## Function checks for and returns a stored inverse;
## if none, returns inverse of matrix stored in makeCacheMatrix 

cacheSolve <- function(x, ...) {
 
  ##checks for value stored in getinverse above, returns value if not null
  
  i <- x$getinverse()  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get() ##creates object with list stored in makeCacheMatrix 
  i <- solve (data) ##calculates inverse of matrix 
  x$setinverse (i) ##stores inverse of matrix 
  i

}