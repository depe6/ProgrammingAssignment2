## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following 2 functions will help in dealing with such problem by
## introducing caching.


## This function creates a special "matrix" object that can cache its inverse.
## The "matrix" object will contain the following functions:
##      set - sets the matrix data
##      get - returns matrix data
##      setsolve - caches inversed matrix
##      getsolve - returns cached inversed matrix
makeCacheMatrix <- function(m = matrix()) {
        cached_solve <- NULL
        set <- function(new_matrix) {
                m <<- new_matrix
                cached_solve <<- NULL
        }
        get <- function() { 
                m
        }
        setsolve <- function(s) {
                cached_solve <<- s
        }
        getsolve <- function() {
                cached_solve
        }
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the cached inverse is available for the specified
## matrix then it will be returned instead of perfoming inverse calculations.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}