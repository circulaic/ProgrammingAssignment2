## Completed by G Beart on 3/05/20 for Week3 Assignment of R Programming on Coursera

## To save on costly computation, these functions help to cache a matrix and retrieve
## it rather than time-intensively computing it multiple times.
## To achieve this, these functions use scoping to assign a value to an object 
##  in an environment that is different from the current environment.

## makeCacheMatrix creates a special matrix containing a function to:
##  1. set the value of the matrix (set)
##  2. get the value of the matrix (get)
##  3. set the value of the inverse (setsolve)
##  4. get the value of the inverse (getsolve)
   
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of a given matrix which is created via the 
## makeCacheMatrix function above. However, if the inverse of this matrix has 
## previously been created, this function instead retrieves this inverse from the
## cache to skip  the costly computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached inverse matrix!")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}
