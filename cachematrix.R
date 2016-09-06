## Caching the inverse of a matrix
## The functions below in conjunction work to return the inverse of a matrix.
## In particular it is aimed at caching the calculated inverse so that it can 
## be quickly retrieved from the first calculation without having to recalculate it.
## Assumptions:
## 1. The matrix supplied is a square invertible matrix
## 2. A special matrix will be created by calling makeCacheMatrix()
## 3. Inverse is obtained by calling cacheSolve() for an instance of makeCacheMatrix()
## 4. Other parameters to the solve() can be passed while calling cacheSolve()
## 5. To obtain cached inverse, the same instance of makeCacheMatrix() to be used



## This function returns a named list of functions to be used by cacheSolve()
## Defines the "getters" and "setters"
## Uses lexical scoping concepts to mutate state in enclosing environments

makeCacheMatrix <- function(x = matrix()) {
        
        inverse_matrix <- NULL
        
        ## function to set the value of the new matrix in the enclosing 
        ## environment from the parent frame and set previous cached value to NULL.
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        ## gets the value of the matrix from enclosing environment
        get <- function() x
        
        ## sets the inverse of the matrix in the enclosing environment
        set_inverse <- function(inv.matrix) inverse_matrix <<- inv.matrix
        
        ## gets the inverse of the matrix from the enclosing environment
        get_inverse <- function() inverse_matrix
        
        ## return a named list for setter & getter functions
        list(set = set, get = get, set_inverse = set_inverse, 
             get_inverse = get_inverse)

}


## Uses as instance of makeCacheMatrix() as input
## Checks if inverse is already calculated and stored in enclosing environemnt (instance of mmakeCacheMatrix)
## Returns cached value if so otherwise caclulates inverse and stores for future

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv.matrix <- x$get_inverse()
        
        if (!is.null(inv.matrix)) {
                
                print("Inverse already calculated. Getting cached result")
                return(inv.matrix)
        }
        
        input_matrix <- x$get()
        
        inv.matrix <- solve(input_matrix, ...)
        
        x$set_inverse(inv.matrix)
        
        inv.matrix
        
}
