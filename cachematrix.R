## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It will not recompute the inverse when the input matrix have no change.
makeCacheMatrix <- function(x = matrix()) {
inver <- NULL    ##no cached yet
set <- function(m){
  x<<- m
  inver <<- NULL    ##reset cache for matrix changing
} ## assign new matrix and clear cached inverse
get <- function() { x } ## retrieve current matrix
setinver <- function(inverse) { inver <<- inverse } ## store inverse
getinver <- function() { inver } ## retrieve inverse
list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## Write a short comment describing this function
## Compute from makeCacheMatrix returned inverse matrix. It returns the cached inverse. If not, it computes the inverse and caches then returns it.
cacheSolve <- function(x, ...) { 
  inver <- x$getinver()
  if (!is.null(inver)) {
    message("getting cached inverse")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...) ## compute inverse
  x$setinver(inver)
  inver
        ## Return a matrix that is the inverse of 'x'
}
