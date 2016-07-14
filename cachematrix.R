## Construct a special object that stores a matrix, and calculates the inverse.
## When the inverse is calculated it is cached, to be retrieved in future
## instead of recalculated.

# Function that generates a list object containing functions which 
# perform the required operations to store/retrieve a matrix
# and calculate/retrieve its inverse.
makeCacheMatrix <- function(x = matrix()) {
   
   # initialize with NULL in the cached inverse
   inverseCache <- NULL
   
   # function to SET the matrix, 
   # will also reinitialize the cached inverse to NULL again
   SET <- function(y) {
      # don't wipe the cache if the matrix isn't really changing
      if(isTRUE(all.equal(x,y))) {
         message("No change to matrix")
      } else {
         #reinitialize the cached inverse, it needs to be recalculated
         inverseCache <<- NULL
      }
      # store the new matrix
      x <<- y
   }
   
   # function GET returns the matrix
   GET <- function() {
      x
   }
   
   # function to calculate the inverse, stores in the inverseCache
   SETINVERSE <- function() {
      inverseCache <<- solve(x)
   }
   
   # function GETINVERSE returns the inverse previously stored
   GETINVERSE <- function() {
      inverseCache
   }
   
   #output list of functions
   return(list(SET = SET, 
        GET = GET,
        SETINVERSE = SETINVERSE,
        GETINVERSE = GETINVERSE))
   
}

#===============================================================================
## Either calculates an inverse, or retrieves from the cache.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   # extract whatever is currently in the inverse cache
   inverse <- x$GETINVERSE()
   
   # check whether it is null (has not been calculated yet)
   if (is.null(inverse)) {
      message("Calculating inverse")
      # if null, then calculate inverse
      inverse <- x$SETINVERSE()
   } else {
      message("Returning cached inverse")
   }
   
   # return the inverse
   return(inverse)
   
}
