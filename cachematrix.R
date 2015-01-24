#The function 'makecacheMatrix' provides cashing-functionality for the inverse of a 
#matrix. 

#input: 'matrix' is a matrix for which the inverse is to be cached

#return: a list containing four functions named 'set', 'get', 'setinverse', 
#'getinverse' to
#    set the value of the matrix and automatcally reset the inverse-value to NULL
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the inverse

makeCacheMatrix <- function(matrix = matrix()) {
    
    inverse <- NULL
    
    set <- function(inputMatrix) {
        matrix <<- inputMatrix
        inverse <<- NULL
    }
    
    get <- function()  matrix
    
    setinverse <- function(inputInverse) inverse <<- inputInerse
    
    getinverse <- function() inverse
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#The function 'cacheSolve' provides the inverse of a matrix. The matrix must be 
#invertable and wrapped by the 'makeCacheMatrix'-function. 'cacheSolve' returns a 
#cached value if existant. Otherwise, it calculates and caches the inverse.

#input: 'matrixWrap' is an invertable matrix wrapped by the 'makeCacheMatrix'-function,  
#i.e. a list of four functions named 'set', 'get', 'setinverse', 'getinverse' providing
#access to a matrix and if existant its cached inverse.

#output: the inverse of the input-matrix as a matrix

cacheSolve <- function(matrixWrap, ...) {
    
    inv <- matrixWrap$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix <- matrixWrap$get()
    inv <- solve(matrix, ...)
    matrixWrap$setinverse(inv)
    
    inv
}
