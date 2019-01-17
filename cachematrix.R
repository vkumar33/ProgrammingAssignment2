## Set function caches the matrix y to x
makeCacheMatrix <- function(x = matrix()){

        m <- NULL
        set <- function(y){
          x <<- y
          m <<- NULL
        }
        
        get <- function() x    ## Get function returns back the matrix x that has been cached
        setsolve <- function(solve) m <<- solve    ## Setsolve function caches the inverse matrix of x
        getsolve <- function() m                  ## Getsolve function returns back the cached inverse
        list(set = set, get = get,
             setsolve = setsolve, getsolve = getsolve)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x,...){
        m <- x$getsolve()  ## Calls back the cached inverse matrix from above
        if(!is.null(m)) {   ## If the called back matrix exists, it will print "getting cached matrix"
                message("getting cached data")
                return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m     ## Returns the solved inverse matrix
  
}