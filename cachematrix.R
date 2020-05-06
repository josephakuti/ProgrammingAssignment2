## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function takes a matrix as an input and then sets 'x' (as matrix) and 'm' (as NULL) objects for the parent environment (cacheSolve). Additionally, it gets or retrieves 'x' or 'm'.

## The list function defines the objects: set, get, setinverse, and getinverse that can be used within the parent environment (cacheSolve). Additionally, this allows these objects to be called using the extract operator (IE: myMatrix$getinverse()). The <<- operator assigns a name for the parent environment, leveraging lexical scoping to find the correct symbol to retrieve the value.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## The cacheSolve function is what does the actual solve() calculation that returns the inverse of 'x'. The cacheSolve function (IE: cacheSolve(myMatrix)) will either: If 'm' is NULL it returns NULL. If 'm' is not null, it returns the cached data stored in 'm'. 



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
