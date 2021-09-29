## makeCacheMatrix creates an object to be passed into cacheSolve(), containing
## a list of getter/setter functions and stored variables 'i' and 'x'

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                          # no inverse calculated yet
        set <- function(y) {               # y must be of type matrix()
                x <<- y                    # a new matrix is active, and
                i <<- NULL                 # the inverse hasn't been calculated
                }
        get <- function() x                # returns the active matrix, 'x'
        setinv <- function(inv) i <<- inv  # value passed from cacheSolve
        getinv <- function() i             # returns inverse value
        list(set = set,                    # list of functions passed to
             get = get,                    # cacheSolve() to maintain link
             getinv = getinv,
             setinv = setinv)
}


## cacheSolve() returns the inverse of 'x' from memory if it has already been
## calculated, or calculates the inverse and stores it to 'i'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()                           # initialize 'i' from arg func
        if (!is.null(i)) {                        # return inverse if available
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()         # if 'i' hasn't been calculated, get data and
        i <- solve(data, ...)   # calculate the inverse of the passed matrix.
        x$setinv(i)             # set 'i' to calculated
        i                       # return calculated inverse value
}
