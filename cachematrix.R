## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
