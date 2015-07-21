## The functions below allow you to calculate the inverse of a matrix and store it in cache.
# If the inverse of the matrix you enter has already been calculated and stored in cache,
# the functions will return the stored inverse of that matrix, if not it will calculate the
# inverse of your matrix, return the invese of your matrix, and 
#cache the inverse of your matrix.


## The first function, "makeCacheMatrix' creates a special "vector", 
# which is really a list containing a function to
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the inverse of a matrix
#4. get the value of the inverse of a matrix


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


## The following function calculates the inverse of a matrix of the special "vector" 
#created with the above function. However, it first checks to see if 
#the inverse  of the matrix has already been calculated. If so, 
#it  get's the inverse of the matrix from the cache and skips the computation. 
#Otherwise, it calculates the inverse matrix of the data and 
#sets the value of the inverse of the matrix in the cache via the 'setsolve' function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
