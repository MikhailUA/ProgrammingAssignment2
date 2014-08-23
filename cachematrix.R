## Functions which cache and inverse matricies
## 

## Function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x

        setSolved <- function(mSolved) s <<- mSolved
        
	getSolved <- function() s
        
	list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)

}


## Function computes the inverse of the special "matrix" object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## If the matrix determinant is 0 then matrix can't by inversed and a message "matrix is not invertible" appears. 

cacheSolve <- function(x, ...) {

        s <- x$getSolved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        
	if(det(data)==0) {
	
	message("matrix is not invertible")	
	
	} else {

	s <- solve(data)
        
	x$setSolved(s)
        
	s
	}
}
