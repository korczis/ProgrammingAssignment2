# This second programming assignment will require you to write an R function 
# that is able to cache potentially time-consuming computations. 
# For example, taking the mean of a numeric vector is typically a fast 
# operation. However, for a very long vector, it may take too long 
# to compute the mean, especially if it has to be computed 
# repeatedly (e.g. in a loop). If the contents of a vector are not 
# changing, it may make sense to cache the value of the mean so that when 
# we need it again, it can be looked up in the cache rather than recomputed. 
#
# In this Programming Assignment you will take advantage of the scoping 
# rules of the R language and how they can be manipulated to 
# preserve state inside of an R object.
#
# For full assignment description see https://github.com/rdpeng/ProgrammingAssignment2

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  inv <- NULL

  # return a list containing all functions defined above
  list(
    # Get value of matrix
    get = get <- function() {
      x
    },
    
    # Set value of the matrix
    set = function(y) {
      x <<- y
      inv <<- NULL # matrix has changed, reassign to NULL
    }, 
    
    # Get inverse of matrix
    getinverse = function() {
      inv
    },
    
    # Set inverse of matrix
    setinverse = function(inverse) {
      inv <<- inverse
    }
  )
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Try to get cached inverse matrix
  inv <- x$getinverse()
  
  # Return cached matrix if found
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Get matrix if cached result not found
  data <- x$get()
  
  # Do heavy computing here - calculate inverse matrix
  inv <- solve(data, ...)
  
  # Store inversed matrix in cache
  x$setinverse(inv)
  
  # And finally return result
  inv
}

## How to use?
# m <- matrix(rnorm(16), 4, 4)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)
