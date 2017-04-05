## The following functions cache the inverse of an invertible matrix.
## Usage example:
##> a <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)) ##creates a special matrix
##> cacheSolve(a)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##On using cacheSolve again, the cached inverse is printed
##> cacheSolve(a)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##The first function, makeCacheMatrix creates a list of functions to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##The following function, cacheSolve, calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If already calculated, it prints the message "getting cached data" and returns the cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setsolve(i)
        i
}
