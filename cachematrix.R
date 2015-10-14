## makeCacheMatrix function creates a list of functions for manipulation with source and inverted matrices
## input parameter is a matrix to be inverted
## usage example: mat<-makeCacheMatrix() or mat<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
# m is a cache variable, it contains resulting inverted matrix
# it is being reset on initialization 
        m<-NULL
# set function accepts matrix for inversion as the input and assign it to x, 
# previously calculated inverse matrix m reset so "getsolved" will not return it from cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
# get function returns input matrix provided either as input parameter for makeCacheMatrix or via "set" function
        get <- function() x
# setsolved assigns inverted matrix to the "cache" variable m
# the function is called from the main cacheSolve function
# not to be used directly as whatever "m" is set, it will be returned as cached result during next call of cacheSolve
        setsolved <- function(solved) m <<- solved
# getsolved returns cached value (inverse matrix) or NULL if it was not yet calculated
        getsolved <- function() m
# creation of the list of functions
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


## cacheSolve is the main function for matrix inversion
## input argument - list of functions created by makeCacheMatrix 
## usage example: cacheSolve(mat)

cacheSolve <- function(x, ...) {
# calling getsolved function to check is result is already cached
        m <- x$getsolved()
# if it's cached - return inverted matrix from cache and exit
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# if it's not cached - load the matrix from list object created with makeCacheMatrix
        data <- x$get()
# invert it
        m <- solve(data, ...)
# cache it
        x$setsolved(m)
# and show result
        m        
}
