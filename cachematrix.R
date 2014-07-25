
## Each of these individual functions within makeCacheMatrix can be used to
## display outputs through get() & getinv(). While the set functions are used
## for setting data

## makeCacheMatrix gives a list of other function viz. get(), set(), etc
## The argument x is a matrix whose data will be cached

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL			
		##initialzing inverse to null evertime we call the makeCacheMatrix function to erase the prior value
        		
			set <- function(y) 	{ if ( is.matrix(y)== TRUE ) {	##set function tests whether the data we have entered is a matrix
                					  x <<- y
                					  inv <<- NULL
							} else { 
			"Object entered isn't a matrix. Please re-enter a matrix" }
        						 }
        get <- function() {x}			##get function prints out the value of the matrix stored in cache

        setinv <- function(inverse){inv <<- inverse}  ##This function is used to manually set an inverse of the matrix that we have entered earlier
									##setinv(inverse) also helps in updating the value of inv in this function when calculated in the cache solve function 
        getinv <- function() inv 

        list(set = set, get = get,      ##This gives us the power to run each of the sub-functions in this main function. We display the sub-functions as a list.
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve aims to print out the inverse of the matrix in the argument of the function
## It checks the cache of the matrix to see if the inverse is already available. If not, it calulates and displays it for u

cacheSolve <- function(x, ...) {
        inv <- x$getinv()		##gets the value of the inverse of the respective matrix from cache
        if(!is.null(inv)) {		##Checks if the inverse value for the respective matrix in not null.
                message("getting cached data")		##Gives an indication of presence of pre-calculated inverse
                return(inv)		##Returns the value of the pre-calculated inverse
        } 

        data <- x$get() 	##this is of consideration only when a pre-calculated inverse is not available
	  inv = solve(data)	##Formula for calulating the inverse of this matrix
		   
        x$setinv(inv)		##Calls a sub-function withing parent function - makeCacheMatrix. Now, the value of the inverse for the respective matrix will be stored in cache.

        inv				##Returns the value of inverse
}
