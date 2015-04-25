#Calculates the iiverse of a matrix and stores it in the cache for later use.

# creates a matric object for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
                # setup of cache                
                m <- NULL 
               
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                # returns matrix
                get <- function() { 
                        x 
                } 
                # store in cache
                setinv <- function(inv) { 
                        m <<- inv
                } 
                # return the chached inverse
                getinv <- function() { 
                        m
                } 
                list(set = set, get = get, setinv = setinv, getinv = getinv)        
}


## Returns a matrix which is the inverse of input matrix'x'

cacheSolve <- function(x, ...) {
              
       m <- x$getinv() #get cached data
       
       if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data,...)
                x$setinv(m)
                m
        }
        
        
# debugcode
#dummy matrix for testing 
#test<-matrix(c(1,2,1,2), nrow=2,ncol=2)
#summary(test)
#test$get()