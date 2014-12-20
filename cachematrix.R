## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x) {      
  
  ma <- NULL    #  ma will be our 'Matrix' and it's reset to NULL every time makeCacheMatrix is called
  
  get <- function() { x }   # this function returns the original value 
  
  setsolve <- function(solve)  { ma <<- solve }
  # this is called by cacheSolve() during the first cacheSolve()
  # access and it will store the value using superassignment
  
  getsolve <- function() { ma } 
  # this will return the cached value to cacheSolve() on subsequent accesses
  
  list(get = get,          #  OK, this is accessed each time makeCacheMatrix() is called,       
       setsolve = setsolve,  #   that is, each time we make a new object.  This is a list of 
       getsolve = getsolve)  #   the internal functions ('methods') so a calling function
  #   knows how to access those methods.                            
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  
  ma <- x$getsolve()               # accesses the object 'x' and gets the value of the mean
  if(!is.null(ma)) {              # if mean was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(ma)                       # ... and return the mean ... "return" ends 
    #   the function cachemean(), note
  }
  data <- x$get()        # we reach this code only if x$getmean() returned NULL
  ma <- solve(data, ...)   # if m was NULL then we have to calculate the mean
  x$setsolve(ma)           # store the calculated mean value in x (see setmean() in makeVector
  ma               # return the mean to the code that called this function
}
