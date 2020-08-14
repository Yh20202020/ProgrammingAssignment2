## Functions that cache the inverse of a matrix

## Function creates special matrix that caches the inverse

makeCacheMatrix <- function(x = matrix()) {inver <- NULL
     set <- function(y){
           x <<- y
           inver <<- NULL
       }
     get <- function() x
     setInverse <- function(solveMatrix) inver <<- solveMatrix
     getInverse <- function() inver
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
     if(!is.null(inver)){
      message("getting cached data")    
      return(inver)
      }
     data <- x$get()
     inver <- solve(data)
     x$setInverse(inver)
     inver      
}
