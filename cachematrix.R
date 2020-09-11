# create matrix object that can
# - get and set internal matrix
# - get and set cached matrix inverse
# - setting a new identical matrix will not override existing cache
makeCacheMatrix <- function(x = matrix()) {
    inverse <<- NULL

    get <- function() x
    set <- function(y = matrix()) {
        if(any(y != x)) {
            x <<- y
            inverse <<- NULL
        }
    }
    getinverse <- function() inverse
    setinverse <- function(y) {
        inverse <<- y
    }

    list(
         set=set, get=get,
         getinverse=getinverse,
         setinverse=setinverse
    )
}


# solves above special cached matrix
# - first tries to get inverse
# - if inverse present (previously calculated), return it
# - if inverse not present, calculate and cache it
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        return(inverse)
    } else {
        inverse <- solve(x$get())
        x$setinverse(inverse)
        return(inverse)
    }
}
