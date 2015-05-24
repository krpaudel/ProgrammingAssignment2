#the function 'makeCacheMatrix' creates a special matrix object in order to catch ist invrse but does not compute it. The 
#function 'cacheSolve' computes the inverse of the the user defince matrix for the 'makeCacheMatrix' function. 

#1. The function 'makeCacheMatrix' involves four differne functions inside it:set, get, setmatrix and getmatrix. Every 

#time use fo the symbol x<<-y instead of x<-y assigns the sybmol y to x in the main function 'makeCacheMatrix' instead of 
#just in m<-NULL and accordingly in m<<-NULL. If 
#In the end 'list' assigns/stores all the four functions within the scope/reach of 'makeCacheMatrix'.

#2. The function 'cacheSolve computes and returns the inverse of the user chose matrix through 'makeCachematrix'.
 # The sttement: if(!is.null(m)){ essage("getting cached data") return(m) ensures that if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#Else, 'cacheSolve' picks matrix data from the user defined new matrix 'm' and calculates inverse of the matris.
#CAUTION: you may get error with this case if your matrix is a singular matrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y # assigns y to x in within the field of makeCacheMatrix, a way of implementing lexical scoping. 
    m<<-NULL
  }
get<-function() x
setmatrix<-function(solve) m<<- solve 
getmatrix<-function() m 
list(set=set, get=get, 
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}
cacheSolve <- function(x=matrix, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
