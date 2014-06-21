makeCacheMatrix <- function(x = matrix()){##creates list of functions which is subset in cacheSolve function
	invmatrix <- NULL
	set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmatrix <<- solve
        getinv <- function() invmatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {##if matrix inverse has not been calculated, calculates matrix inverse and stores in global envinroment. If calculated previously retrieves previous result from global environment
        invmatrix <- x$getinv()
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinv(invmatrix)
        invmatrix
}