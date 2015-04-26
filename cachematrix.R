## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create the cache

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL #to store the result of inverse matrix
        # set function, to set a matrix to object created by makeCacheMatrix
        set <- function(y) {
	x <<- y
	xinv <<- NULL 
      }

      get <- function() x # to get the test input matrix
      setInv <- function(inv) xinv <<- inv #to set the inverse matrix
      getInv <- function() xinv #to return the inverse matrix
      #to return a list which contains these functions
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
}


## cacheSolve to create th inverse of the cached matrix

cacheSolve <- function(x, ...) {
    m <- x$getInv() # get the inverse matrix from x
    # we set this to null earlier if not already set
    if(!is.null(m)) { # to check for the inverse, if it exists
    message("getting cached matrix")
    return(m) # return the inverse
    }
    data <- x$get() 
    m <- solve(data) 
    x$setInv(m) 
    m 
}


# To test the code
  # simple matrix
  test<-matrix(1:10,2,2)
  test
  # run makeCacheMatrix 
  testCached <- makeCacheMatrix(test)
  # retrieve inverse using cacheSolve

  testInv <- cacheSolve(testCached)
  testInv
  
