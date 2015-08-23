##Functions calculate the inverse of a matrix.

# Creates a cached matrix accessible to cacheSolve that has four properties:
# assigns a global variable x the value y, get to return the parameter value x,
# setinverse to caculate the 
# inverse of x, and getinverse to return the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function () m
        list (set = set, get = get, setinverse = setinverse,
              getinverse = getinverse)
}

#Creates a function that takes the matrix returned by makeCacheMatrix and returns
#its inverse.

cacheSolve <- function(x, ...) {
        ## Retrieves the inverse of makeCacheMatrix, if it exists.
        m <- x$getinverse()
        
        ## If the inverse doesn't already exist, calculates the inverse and
        ## returns it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
