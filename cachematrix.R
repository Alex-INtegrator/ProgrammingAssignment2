## These functions let you to calculate and cache the inverse of a square and 
## invertible matrix 

## This first function "makeCacheMatrix" creates a list containing 4 functions
## to be used in order to SET and GET the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # 1. function to SET the value of the matrix
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        # 2. funtion to GET the value of the matrix
        get <- function() x
        # 3. function to SET the value of the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
        # 4. function to GET the value of the inverse of the matrix
        getinv <- function() inv
        # Finally creates the list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function "cacheSolve" evaluates if the inverse of the matrix 
## has already been cached and it returns it. If not,it calculates, caches 
## and returns the inverse using the functions above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() #gets the value of the inverse
        if(!is.null(inv)){ #evaluates if that value is not null
                message("getting cached data") 
                return(inv) # if it is then returns cached inverse 
        }
        data <- x$get()
        inv <- solve(data, ...) #else calculates inverse of the matrix
        x$setinv(inv) # caches the new value of the inverse
        inv #returns the new value
}
