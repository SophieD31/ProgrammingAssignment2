## This pair of functions is used to cache the inverse of a matrix.


## This function creates a special "matrix" object that stores a matrix and can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the special "matrix" created with 
## function makeCacheMatrix.
## First it checks if the inverse has already been calculated for this matrix and
## in this case retrieve the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse of the matrix and sets the inverse in the 
## cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
