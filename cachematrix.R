## Put comments here that give an overall description of what your
## functions do

## simple change

## Write a short comment describing this function

# The function makeCacheMatrix accepts a matrix and returns 
# a list for the following operations
#   1. set - to set to a new matrix, other than the initialized one
#   2. get - to return the initialized matrix
#   3. setinv - to set the inverse of the matrix that is set
#   4. getinv - to get the inverse of the matrix that is set
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (y) {
        # set the variable x, that is defined in the parent environment of
        # of this function, also set the inverse to NULL when the value
        # is initialized
        x <<- y
        inv <<- NULL
    }
    
    # return the matrix that is stored
    get <- function() {
        return(x)
    }

    # set the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse
    
    # return the inverse matrix that is saved
    getinv <- function() {
        return(inv)
    }

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



# The function cacheSolve() takes the return value of makeCacheMatrix as a 
# parameter and returns the inverse of the matrix that it is created with
# The inverse matrix is cached and is not computed if the function is called
# again. The function checks to see if the inverse is NuLL, if it is, then
# the inverse of the matrix is computed and cached. If it is not NULL, then 
# the cached inverse is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # check whether the inverse is cached already?
    # if null, then compute the inverse and cache it
    if (is.null(inv)) {
        message("first time, computing inverse")
        x$setinv(solve(x$get()))
        inv <- x$getinv()
    }
    return(inv)
}



# to see the program in action, after sourcing this file, type
# runCacheAssignment() in the interactive prompt
runCacheAssignment <- function () {
    message(" First creating a simple diagonal matrix")
    print(5 * diag(4))
    message(" Calling tmp <- makeCacheMatrix(5*diag(4))")
    tmp <- makeCacheMatrix(5*diag(4))
    
    message(" Output of tmp$get() ")
    print(tmp$get())
    
    message(" Calling the inverse directly first to verify it is empty ")
    message(" Calling tmp$getinv() ")
    print(tmp$getinv())
    
    message(" Calling cacheSolve(tmp), will print first time, computing inverse ")
    print(cacheSolve(tmp))
    
    message(" Calling the inverse directly, it is not empty this time ")
    message(" Calling tmp$getinv() ")
    print(tmp$getinv())
    
    message(" Calling cacheSolve(tmp), no messages, this is now cached ")
    print(cacheSolve(tmp))
}
