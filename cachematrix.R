## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# creates a matric object for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL # setup of matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(inv) m <<- inv
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        
}


## Returns a matrix which is the inverse of input matrix'x'

cacheSolve <- function(x, ...) {
              
       m <- x$getinv()
       if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
                m
        }
        
        
# debugcode
#dummy matrix for testing 
test<-matrix(c(1,2,1,2), nrow=2,ncol=2, byrow=TRUE)