## This function is expected to cache the inverse of matrix(rnorm(12),3,3,3))

makeCacheMatrix <- function(x = matrix(rnorm(12),3,3,3)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Below computes the inverse of the matrix above
cacheSolve <- function(x = matrix(rnorm(12),3,3,3)) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("computing cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

### Validation 
##mat <- matrix(rnorm(12),3,3,3)
##mat1 <- makeCacheMatrix(mat)
##cacheSolve(mat1)




