## This function caches the inverse of a matrix thereby saving time for runnign the programm
## The "makeCacheMatrix" function gets and sets a matrix and its inverse
## Various function include :- get() - to get the matrix,  set() - to assign values to matrix object
## getinverse() - to get the inverse of a matirx from cachesolve function, setinverse() - to set inverse of a matrix from cache solve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## This function checks for existance of the inverse matirix, if NULL it will proceed to create inverse using solve(x) function
## which later will be stored for further use without the necessity to claculate it again and again.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i

        ## Return a matrix that is the inverse of 'x'
}
