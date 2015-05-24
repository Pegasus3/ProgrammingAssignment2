##  makeCacheMatrix function takes advantage of R's lexical scoping rules to
##  store (cache) the results of time-consuming calculations thereby increasing
##  the efficiency and speed of R programming.
##  Here, makeCacheMatrix caches the result of an inverted matrix.
##  makeCacheMatrix function stores a list of four other functions designed to:
##  1.  Set a new matrix - if a new matrix is passed to the function the cached matrix is erased.
##  2.  Get the existing matrix
##  3. Set the inverse matrix and
##  4. Get the inverse matrix
##  In addition, if a new matrix is passed directly to the makeCacheMatrix function 
##  the previous cached matgrix is also erased.     

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                   #  Set a new matrix
        x <<- y
        m <<- NULL
    }

    get <- function() {                    #  Get the existing matrix
        x
    }

    setinverse <- function(inverse) {      #  Set the inverse matrix
        m <<- inverse
    }
    getinverse <- function() {             #  Get the inverse matrix
        m
    }                                      # The list of 4 functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##  Function cacheSolve recieves a matrix from the makeCacheMatrix function
##  and returns its inverse.  The cacheSolve function inverses the matrix - However,
##  it first checks to see if the inverse has already been calculated.  If so, it gets
##  the inverse from the cache and skips the computation.  Otherwise, it reverses
##  the matrix and sets the value of the inverse in the cache by way of the
##  setinverse function.

 cacheSolve <- function(x, ...) {
     m <- x$getinverse()                # Get inverted matrix 
     if(!is.null(m)) {                  #  if m is not NULL, then return cached data  
         message("getting cached data") # Message of cahed data 
         return(m)                      # And exit function
     }
     data <- x$get()          # Otherwise, get matrix
     m <- solve(data, ...)    # Invert it
     x$setinverse(m)          # Make it available to the setinverse function
     m                        # return inverted matrix

 }
