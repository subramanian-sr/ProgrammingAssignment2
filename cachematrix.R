# Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		
		k <- NULL
		set <- function(y) x <<- y
			
		get <- function() x
	
        	setinverse <- function(inverse) k <<- inverse
      	
		getinverse <- function() k
        	
		list(set = set, get = get,
            	 setinverse = setinverse,
             	getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, z = makeCacheMatrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
		j <- z$get()
		
		print(x)
		print(j)
		if(identical(x,j)) {
			print("identical")
			k <- z$getinverse()
			print(k)
			if(!is.null(k)) {
				print("getting cached data")
				return(k)
			}
		}
		
		k <- solve(x)
		print(k)
		z$setinverse(k)
		z$set(x)
		k
}	
