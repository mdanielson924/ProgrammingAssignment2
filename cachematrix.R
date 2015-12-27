## Caching heavy compution (matrix inverse)


## We create a special object we can then use to cache the results of a matrix inverse.  
makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(m){
        x <<- m
        v <<- NULL
    }
    get <- function() x
    setinv <- function(solve) v <<- solve
    getinv <- function() v
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Given the special object we created in 'makeCacheMatrix' we can either solve the inverse or recover the previously computed inverse of a matrix.

cacheSolve <- function(x, ...) {
    v <- x$getinv()
    if(!is.null(v)){
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data)
    x$setinv(v)
    v
}  

