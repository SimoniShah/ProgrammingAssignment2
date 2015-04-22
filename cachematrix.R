## These two functions are used to create a special matrix object 
## that may be used to manage and cache the inverse, 
## so that it is computed only once.

## The function makeCacheMatrix takes a matrix object as an argument 
## and returns a list of functions that are used to set ang get
## the matrix itself, as well as its inverse (special matrix object)

makeCacheMatrix <- function(x = matrix()) {
  invmat=NULL
  
  setMat<-function(m)
  {
    x<<-m
    invmat<<-NULL   #invmat is the inverse matrix.
  }
  getMat<- function()
  {
    x
  }
  setInv<- function(m)
  {
    invmat<<-m
  }
  getInv<- function()
  {
    invmat
  }
  list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)
}


## cacheSolve is a function which takes the special matrix object 
## (that is returned by the makeCacheMatrix function) 
## and returns the inverse matrix. 
## If the inverse is qureyed for the first time, it is computed and cached.
## For future queries, the inverse is retrived from the cache.

cacheSolve <- function(x, ...) {
  tempObj <- x$getInv()
  if(is.null(tempObj))
  {
    print("Inverse not computed yet")
    tempInv=solve(x$getMat())
    x$setInv(tempInv)  
  }
  else
  {
    print("Inverse already computed")
  }
  x$getInv()
}
