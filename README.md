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


### makeCacheMatrix creates an object of class "matrix" that can cache its inverse.
## Assume that the matrix is square and invertible.

### cacheSolve takes the created matrix object and calculates the inverse 
## if the inverse has been calculated, that will be retrieved from the cache.
