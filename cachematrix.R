## The following functions are used to solve a square invertible matrix. The intent is to
## cache the matrix inverse, once solved, so that we don't waste resources unnecessarily
## by solving the same item over and over.

## This function creates a CacheMatrix object, with get/set functions for both the matrix
## and its inverse (solution). The object contains the following get/set functions:
##
## 1. set()        - Sets the matrix value. This is optional unless an existing value is
##                   being reset, as it's performed automatically during object creation.
## 2. get()        - Returns the currently set matrix value.
## 3. setinverse() - Sets the matrix inverse (solution) value.
## 4. getinverse() - Gets the currently set matrix inverse value.
##
## The initial matrix value is passed in the 'x' parameter.
##
makeCacheMatrix <- function( x = matrix() ) {
    ## It appears that the 'x' parameter becomes an implicit variable within the object
    ## created by this function. This behaviour is rather confusing, however, so I'm
    ## making the initial matrix assignment explicit for readability.
    matrix_orig <- x
    
    # matrix inverse value, NULL until assigned via "setinverse"
    matrix_inv <- NULL
    
    set <- function( mtrx = matrix() ) {
        matrix_orig <<- mtrx
        matrix_inv  <<- NULL   ## reset to NULL since the corresponding matrix has been reset
    }
    
    get <- function() matrix_orig
    
    setinverse <- function( inverse ) matrix_inv <<- inverse
    
    getinverse <- function() matrix_inv
    
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse
        )
}


## This function leverages makeCacheMatrix above, in order to avoid computing the inverse 
## if the given matrix has already been solved. It will simply return the cached value in
## that case, and will compute and cache the inverse otherwise.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if ( !is.null( inverse ) ) {
        message( "getting cached data" )
        return( inverse )
    }
    
    data    <- x$get()
    inverse <- solve( data, ... )
    x$setinverse( inverse )
    inverse
}
