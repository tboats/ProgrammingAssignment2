## Put comments here that give an overall description of what your
## functions do

## Stores a user input matrix via "set" and its inverse via "setinv"
# the matrix and its inverse can be extracted via the "get" and "getinv" functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## This function checks if the inverse for the given matrix exists in the user input makeCacheMatrix object
# If the matrix and inverse exist, the cached inverse is output, without recalculating
# If the matrix does not exist, the inverse is calculated via "solve" and stored in the makeCacheMatrix object via "setinv"


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


# # test code
# mat<-matrix(c(1,1,1,2),nrow=2,ncol=2)
# test<-makeCacheMatrix(mat)
# test$get()
# test$getinv()
# cacheSolve(test)

