## To improve performance, if the inverse of a matrix is calculated once then it 
## is cached and stored with the matrix object as a "cached matrix" object

## The makeCacheMatrix() function initialises the "cached matrix" and includes
## a list of getters and setters for the matrix object as well as getters and 
## setters for any possible cached inverse matrix (if it has been calculated)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve() function calculates the inverse of any "cached matrix" object
## If the inverse is not cache it calls R's solve() function and caches the 
## inverse with the "cached matrix".
## If the inverse is cached and the values haven't changed then that cached 
## inverse matrix is returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

unitTests <- function() {
    
    test_matrix <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
    magic_matrix <- makeCacheMatrix(test_matrix)
    
    ## Test that the get() method returns the initialisation matrix
    expected <- test_matrix
    actual <- magic_matrix$get()
    
    print(paste("Matrix returned from magic_matrix get() method is the test_matrix: ", 
                identical(actual,expected)))
    
    ## Test that the inverse is calculated correctly
    expected <- diag(2)   ## 2x2 identity matrix
    inverse <- cacheSolve(magic_matrix)
    actual <- test_matrix %*% inverse   ## Matrix * Inverse = identity matrix
    
    print(paste("The cacheSolve() function returns the inverse of the test_matrix: ", 
                all.equal(actual, expected)))
}
