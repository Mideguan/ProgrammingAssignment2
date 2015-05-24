## NOTES:
## This R program is able to cache potentially time-consuming computations by
## taking advantage of the scoping rules of the R language to preserve state 
## inside of an R object. 
## Limitations: This function assumes that the matrix supplied is always invertible.


## The 1st function, makeCacheMatrix creates a special "matrix", a list  
## containing a function which
##      (i)  sets the value of the matrix
##      (ii) gets the value of the matrix
##      (iii)sets the value of the inverse
##      (iv) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                
                ## The '<<-' operator used to assign a value to an object in 
                ## an environment that is different from the current environment
                
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinvm <- function(inv) i <<- inv
        getinvm <- function() i
        
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}


## The 2nd function, cacheSolve, computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinvm()
        
        ## Check from cache if the inverse has been computed earlier
        if(!is.null(i)) {
                
                ## If computed earlier, get from cache 
                message("getting cached data")
                return(i)
        }
        
        ## If NOT computed earlier, compute inverse 
        matrixtrinity <- x$get()
        i <- solve(matrixtrinity, ...)
        
        ## Sets the inverse value in the cache via the setinvm function         
        x$setinvm(i)
        i
}
