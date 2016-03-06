
# Returns a list of the following functions
# set : sets the value of the elements of a matrix
# get : gets the elements of the matrix
# setinverse : sets the inverse of a matrix
# getinverse : gets the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize a variable that holds the inverse matrix
    # to null
    mInv <- NULL
    # create a set function for the  matrix
    set <- function(y) { 
        x <<- y
        mInv <--  NULL
        }
    # create a get function for the matrix
    get <- function() x
    # create set function for the matrix inverse
    setinverse <- function(inverse) mInv <<-inverse
    # create a get function for the matrix inverse
    getinverse <- function() mInv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#Returns the inverse of a matrix
# checks if the inverse is already in the cache, if so returns ir
# else, calculates the inverse, sets it in the cache and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("getting matrix inverse from cached data")
        return(inv)
    }
    message("matrix inverse is not in the cache, so calculating")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}