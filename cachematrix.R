## The following two functions will take a matrix and calculate its inverse using
## the solve function.  The inverse matrix is then cached. If the function is asked 
## to find the inverse of the same matrix again, it will return the cached version
## instead of re-computing the inverse 


## The makeCacheMatrix function takes the input matrix and returns a list with
## four functions to: 
##      set value of matrix
##      get value of matrix
##      set value of the inverse (caches it)
##      get value of inverse (returns cached value)

makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse (cached)
        setsolve <- function(solve) m <<- solve
        
        ## get the value of the inverse (is Null if not yet cached)
        getsolve <- function() m
        
        ## return a list containing the four functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve takes as its input the list created by makeCacheMatrix and checks
## to see if the inverse has already been cached, if so it returns the cached matrix
## if not, it solves the matrix to calculate the inverse

cacheSolve <- function(x, ...) {

        ## checks if inverse has been cached in makeCacheMatrix, if so returns it
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## if m is Null (i.e. no cached inverse, it will solve the matrix)        
        data <- x$get()
        m <- solve(data, ...)

        ## caches the inverse in makeCacheMatrix
        x$setsolve(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
