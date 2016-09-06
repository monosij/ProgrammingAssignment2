## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inverse_matrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inv.matrix) inverse_matrix <<- inv.matrix
        
        get_inverse <- function() inverse_matrix
        
        list(set = set, get = get, set_inverse = set_inverse, 
             get_inverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv.matrix <- x$get_inverse()
        
        if (!is.null(inv.matrix)) {
                
                print("Getting cached inverse")
                return(inv.matrix)
        }
        
        input_matrix <- x$get()
        
        inv.matrix <- solve(input_matrix, ...)
        
        x$set_inverse(inv.matrix)
        
        inv.matrix
        
}
