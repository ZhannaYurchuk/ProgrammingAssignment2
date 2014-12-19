## Matrix inversion is usually a costly computation and their may 
##be some benefit to caching the inverse of a matrix rather than 
##compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        solveM <- NULL
        set <- function(y) {
                x <<- y
                solveM <<- NULL
        }
        get <- function() x
        setsolveM <- function(solve) solveM <<- solve
        getsolveM <- function() solveM
        list(set = set, get = get,
             setsolveM = setsolveM,
             getsolveM = getsolveM)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolveM()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        dataM <- x$get()
        s <- solve(dataM, ...)
        x$setsolveM(s)
        s       
}
