##///---------------------------------------------------------------------------------------------------------
## The functions in this file, 'makeCacheMatrix' and 'cacheSolve', create a special 'matrix' object (actually 
## a matrix stored in a list), solve for its inverse, and cache the inverse matrix in memory.  The caching
## of the inverted matrix allows it to be called later, avoiding costly recalculation.  'makeCacheMatrix' 
## must be called first and takes a matrix as input.  'cacheSolve' on the resultant list object returns the 
## input matrix's inverse and stores it in the list object.  
## E.g.
## 
##> a <- matrix(1:4,2,2)
##> b <- makeCacheMatrix(a)
##> cacheSolve(b)
##> b$get()             <<< returns the original matrix a
##> b$getinv()          <<< returns the inverted matrix a^-1
##
## If the matrix a is singular, 'cacheSolve' will throw an error from the solve function. 
## Note: Be careful to give as an argument to 'cacheSolve' the LIST object b created by 'makeCacheMatrix' 
## and NOT on the original matrix a.
## --------------------------------------------------------------------------------------------------------


## 'makeCacheMatrix' takes as input a matrix x and stores it, in a list format, as well as its inverse, x^-1, 
##once the latter has been returned via the 'cacheSolve' function below.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## 'cacheSolve' calculates the inverse (if it exists) of the input matrix stored in the list x created by 
## the makeCacheMatrix function above. Once the inverse is calculated it is stored in the x list object; 
## further calls of this function on the same list return the cached value rather than recalculating its
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

##---------------------------------------------------------------------------------------------------------///