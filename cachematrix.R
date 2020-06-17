## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function stores the inverse of the matrix in the cahce like the one provided 
#as an example
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function () m
  list(set=set, get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function
#This function checks for the inverse if not in cache, it calculates it
#To use this function: cacheSolve(makeCacheMatrix(x)) 
#e.g. 
# matrix x is:
#    1    2
#    3    4
# cacheSolve(makeCacheMatrix(x))
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data,...)
  x$setinv(m)
  m
}