

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

   ## initialize inverse of matrix with NULL value
   inverse_matrix <- NULL  
   
   ## set matrix with y 
	setmatrix <- function(y) { 
    x <<- y          
    inverse_matrix <<- NULL
   }
   
   ## get matrix 'x'
	getmatrix <- function() x 
   
   ## set inverse matrix  
    setinversematrix <- function(inversematrix) inverse_matrix <<- inversematrix 
   
   ## get inverse matrix 
	getinversematrix <- function() inverse_matrix
	
   ## function return list containing above four method 
   list(setmatrix = setmatrix, getmatrix = getmatrix,setinversematrix = setinversematrix,getinversematrix = getinversematrix)


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {


    ## get inverse matrix using getinversematrix() which is defined into makeCachematrix() function
	m <- x$getinversematrix()
    
	    ## check inverse of matrix is null or not..   
		if(!is.null(m)) {
		    ## If not null then it is cache value
			message("getting cached data")
			## return cached value
			return(m)
			}
	## get matrix using getmatrix() method which is defined in makeCache matrix() function	
	data <- x$getmatrix()
	## find inverse of matrix using solve() method
	m <- solve(data)
	## set inverse of matrix 
	x$setinversematrix(m)
	## Return a matrix m that is the inverse of 'x'
	m  
}
