## Put comments here that give an overall description of what your
## functions do

#The two functions make a matrix like object
#What sets the object apart from matrices is that it's inverse is only solved once.
#The inverse is cached for quick access in later calls. Storage vs. Time where storage is favored

## Write a short comment describing this function

#makeCacheMatrix initalizes the matrix like object and must be called before cacheSolve
#The function contains methods for the object, get(data), set(data, for reset) setinv and getinv.
#The function does no calculation but rather defines how to interact with the object

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function()inv
    list(set=set, get=get, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

#cacheSolve calculates the matrix object inverse once and only once
#I checks if the inverse has been solved, if not: do so and asign it to the object
#If it has been solved (not first call of function for object) it accesses the inverse from memory
#The actual inversion takes place here, but only once

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        print('Getting cached data!')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
