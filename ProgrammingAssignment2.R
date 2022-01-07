## code written for Coursera R programming assignment 3, completed by Chelsea Cole

makeCacheMatrix <- function(x = matrix()) {
  ##this function creates a special "matrix" object that can cache its inverse
  
makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix" 
    inv <- NULL ## initialize inv as NULL
    set <- function(y) { ## define the set function
        x<<- y ## value of matrix in parent environment
        inv <<- NULL ## if there is a new matrix, reset inv to null
    }
    get <- function() X ## define the get function
    
    setinverse <- function(inverse) inv <<- inverse ##assigns value of inv in parent environment 
    getinverse <-function() inv ## get the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## for refering to functions with the $ operator
    
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
