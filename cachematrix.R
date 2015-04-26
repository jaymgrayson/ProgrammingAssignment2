## This creates a function to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        #The next line sets the vaule of m to NULL
        m <- NULL
        set <- function(y) { ## creates a function set
                x <<- y ## This caches the matrix
                m <<- NULL
        }
        get <- function() x ## These next three lines create the get,setmatrix 
        ##and getmatrix functions. Note setmatrix sets the inverse
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, ## This creates a list to hold these functions
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
cacheSolve <- function(x, ...) { ##This creates a cacheSolve function that 
        ##computes the inverse of the special "matrix" returned by 
        ##makeCacheMatrix above. 
        #If the inverse has already been calculated (and the matrix has not 
        ##changed), then the cachesolve should retrieve the inverse from the 
        ##cache.
        m <- x$getmatrix() ## This retrieves the value of a previously calculated
        ##matrix
        
        if(!is.null(m)) { ##this checks to see if the previous function has been
                ##run
                message("getting cached data")
                return(m)
        } ## The next block of code calculates the inverse matrix if it has not
        ##been done before,
        matrix<- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}