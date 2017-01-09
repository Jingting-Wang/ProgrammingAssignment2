## The two functions below can return the converse matrix of input matix and if
## the inverse matrix has already been calculated, the function will directly
## return the caches values.

## The 'makeCacheMatrix' function make a list to identify input matrix and cache
## calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
        getInverseMatrix <- function() m
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## The 'cacheSolve' function return the inverse of input matrix and do essential
## calculation if needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(! is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
