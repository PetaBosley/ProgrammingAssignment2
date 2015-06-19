## Programming Assignment 2: Caching the Inverse of a Matrix
##
## The assignment consists of 2 function; makeCacheMatrix and cacheSolve (each shown and 
## commented below in this file). 
## The function code was unit tested in the R console by setting up a matrix, assigned to x 
## (see below)
##
## x <- matrix(c(1,2,3,4), 2, 2)
## > x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## The functions were then executed and assigned to z (see below)
## checking for the expected, inverted result when z was printed to the console.
##
## > z <- cacheSolve(makeCacheMatrix(x))
##
## > z
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## makeCacheMatrix creates an object of class "matrix" that can cache its inverse.
## Assume that the matrix is square and invertible.


makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL                    ## has invs (inverse) been calculated yet?
        set <- function(y)      {       ## if yes, get from cache
                x <<- y
                invs <<- NULL           ## set inverse to NULL in parent environment
                        }
        get <- function() x
        setinv <- function(solve) invs <<- solve   ## set inverse of matrix
        getinv <- function()invs                   ## get inverse of matrix
        list (set = set, get = get,                ## create function list so all 4
              setinv = setinv,                     ## functions are available to matrix object
              getinv = getinv)
        
}


## cacheSolve takes the created matrix object and calculates the inverse 
## if the inverse has been calculated, that will be retrieved from the cache.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinv()
        if(!is.null(invs))  {                      ## has the inverse matrix been written to cache
                message("getting cached data")
                return(invs)
                
        }
        mtx <- x$get()                            ## assign x to "mtx"
        invs <- solve(mtx, ...)                   ## assign inverse to invs using solve function
        x$setinv(invs)                            
        invs
}

