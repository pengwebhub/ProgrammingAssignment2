## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
                x <<- y
                m <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) m <<- inverse
          getInverse <- function() m
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
    }



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. When it was found calculated before, it will retrieve 
##from cache stored in dummy varialble m.
## otherwise, it will calculate and cache it 

cacheSolve <- function(x) {
        m <- x$getInverse()
        if(!is.null(m)) {
              message("getting cache data")
              return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


