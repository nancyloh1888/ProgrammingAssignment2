## MakeCacheMatrix function is to create a matrix which can cache its
## inverse, the output is used as input of cacheSolve

makeCacheMatrix <- function(x = matrix()) {
		## Create a special matrix which is a list containing a 			## function to
		## 1. set the value of the matrix
		## 2. get the value of the matrix
		## 3. set the value of the inverse
		## 4. get the value of the inverse
		
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special matrix 
## created with makeCacheMatrix functon

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i = x$getinverse()
        
        # if the inverse has been calculated and cached, get it from 		# cache
        
        if (!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        # if not, calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
