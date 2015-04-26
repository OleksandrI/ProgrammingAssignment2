## Functions in this module are designed to make inverse matrix computation faster: they make it possible to start

## creates an "object" that stores initial and inverse matrixes, and also functions to save/read them 

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL # initially, inverse matrix is set to NULL, 'initial' matrix is saved as 'x' (argument)
     set <- function(y) {
          x <<- y # setting new 'initial' matrix
          inv <<- NULL # NULLifying the inverse matrix
     }
     get <- function() x # return matrix used for invese one calculation
     setinv <- function(inverse) inv <<- inverse # save inverse matrix
     getinv <- function() inv # return stored inverse matrix
     # making list to make function of "Matrix" object accessable
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## returns a saved inverse matrix or if no such one calculates it and saves into "Matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinv() # access "Matrix" object and retrieve inverse matrix (if any)
     if(!is.null(m)) { # inverse matrix exists (not NULL) then return it 
          message("getting cached data")
          return(m)
     }
     data <- x$get() # else retrieve 'initial' matrix
     m <- solve(data) # calculate inverse one
     x$setinv(m) # save inverse matrix to "Matrix" object
     m # return calculated inverse matrix
}
