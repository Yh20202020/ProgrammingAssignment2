makeCacheMatrix <- function(a = matrix()) {
  inver <- NULL
  set <- function(n){
    a <<- n
    inver <<- NULL
  }
  get <- function() a
  setInverse <- function(solveMatrix) inver <<- solveMatrix
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(a, ...) {
  inver <- a$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- a$get()
  inver <- solve(data)
  a$setInverse(inver)
  inver      
}
