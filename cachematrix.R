# Here is the function that creates a special "matrix" object that can cache its inverse 
# and other function that computes the inverse of the special "matrix" 
# returned by first one.

# This function creates a special matrix object that can cache its inverse
# which is really a list containing a function to 
#              1. set the matrix
#              2. get the matrix
#              3. set the inverse
#              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special matrix object that can cache its inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# This function computes the inverse of the matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
