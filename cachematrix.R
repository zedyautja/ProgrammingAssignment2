#Accept a square matrix that can have the inverse calculated - instructions explicitly state that I can
# assume this - therefore don't have to handle situations where this is not true

makeCacheMatrix <- function(x = matrix()) {    #input x will be a matrix
        i <- NULL                              # reset i each time. i will be the inverse matrix
        
        get <- function() x                    # returns the value of first matrix
        setinv <- function(solve) i <<- solve  #called by cachesolve on first run, and cache the value for i
        getinv <- function() i                 # return the cached value of i if it exists
        list(get = get,                        #list of the functions which can be used in cacheSolve
             setinv = setinv,
             getinv = getinv)
}

# compute the inverse of the matrix object returned by makeCacheMatrix above
# If the inverse has already been calculated then rtn the cached value and bypass the calculation phase

cacheSolve <- function(x, ...) {               # the input is an object created by makeCacheMatrix
        i <- x$getinv()                        # accesses the object 'x' and gets the value of the inversion
        if(!is.null(i)) {                      # if the i is not NULL, therefore it is cached 
                message("getting cached data") # retn. message to console
                return(i)                      # retn. the cached value for i
        }
        data <- x$get()                        # assigns the value of matrix x to data
        i <- solve(data, ...)                  # solves data and assigns to i
        x$setinv(i)                            # uses setinv from makeCacheMatrix to cache the result using <<-
        i                                      # prints the result to console
}




#test lines, uncomment any 1 of the first 4, and the last 2 lines to test
#mat <- matrix(rnorm(4),2,2) 
#mat <- matrix(1:4,2,2)
#mat <- matrix(rnorm(100),10,10)
#mat <- matrix(rnorm(10000),100,100)
#test <- makeCacheMatrix(mat)
#cacheSolve(test)
