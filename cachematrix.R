## makeCacheMatrix() stores a matrix a its inverse. The inverse is not computed
## directly. It has to be computed by the second function cacheSolve().

## makeCacheMatrix() is a function that has four features. setMatrix changes the
## cached Matrix and getMatrix prints it out. setInverse is later used to
## store the inverse. It doesn't work without the input of cacheSolve(). getInverse
## prints out the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <<- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) inverse<<- solve
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve() is supposed to compute the inverse of the matrix stored in
## makeCacheMatrix(). First it looks if the inverse has already been computed
## and stored in makeCacheMatrix(), if so it returns the stored inverse. If not
## it computes the inverse and returns it to makeCacheMatrix().

cacheSolve <- function(x, ...) {
        inverse<-x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        matrix<-x$getMatrix()
        inverse<-solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
