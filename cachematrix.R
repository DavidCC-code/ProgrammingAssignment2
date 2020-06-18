## makeCacheMatrix, calculates the inverse of matrix x and cache it 

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


## cacheSolve, given a matrix created as inverse cacheable means of makeCacheMatrix, calculates
##   its inverse or retrive it from cache if the original matrix hasn't changed since last cached.
##   If it has been changed the chached inverse has been set to NULL by function set, so there's no
##   need to check for identity of the matrix, plus in order to do that set function should save 
##   old values. 

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
