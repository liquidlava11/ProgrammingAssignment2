## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) 
  {
       m <- NULL    ##Sets value of matrix m NULL as a placeholder for future value
       set <- function(y) 
         {
            x <<- y
            m <<- NULL
        }
       
       ##defines a function to set the matrix, x, to a new matrix, y, and resets the matix, m, to NULL
       
       get <- function() x   ## return the matrix x
       setsolve <- function(solve) m <<- solve   ## sets the matrix m
       getsolve <- function() m   ##gets the matrix m
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)   ## Retruns the special matrix
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

    cachesolve <- function(x, ...) {
            ##returns current version of inverse
            m <- x$getsolve()
           
            ##If inverse is already computed, return cache
            if(!is.null(m)) {    
                  message("getting cached data")
                       return(m)
             }
           
           ##computes and caches inverse
              data <- x$get()
              m <- solve(data, ...)
              x$setsolve(m)
              m
}



