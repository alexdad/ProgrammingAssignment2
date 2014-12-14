## To avoid repeated matrx inversion, use the envelope object, caching the inverse
## First create this object for a given matrix; then use casheSolve function to get it inverted 
## This also works for more general solving of linear systems, since ... allows to pass parameter 'b' to the solve
##
## Usage (assuming square matrix ms)
##	Creating envelope mms for the matrix ms
##		mms<-makeCacheMatrix(ms)
##	Getting inverse of ms. Note tracing "calculated"
##		mis<-cacheSolve(mms) 
##	Getting inverse of ms. Note tracing "getting cached"
##		mis<-cacheSolve(mms) 
##	Testing that the result is a diagonal ones matrix
##	ms%*%mis

## Defines caching object for a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
## Note - it is critically important to use inv<<-inverse and not inv<-inverse, 
##        since the caller (cacheSolve) will need it to x$setinv(inv)
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Returns an inverted matrix for 'x', using cache if it already did it in the past
cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating inverse")
    mt<-x$get()
    inv<-solve(mt,...)
    x$setinv(inv)
    inv
}
