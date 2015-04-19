## Put comments here that give an overall description of what your
## functions do

## below are two functions that are used to create a special object that stores a matrix and caches its inversion.

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special “matrix",which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion
makeCacheMatrix <- function(x=matrix()) {
  inv<-NULL;
  set<-function(y){
      x<<-y;
      inv<<-NULL;
  }
  get <- function() x
  setinverse<-function(solve)inv<<-solve
  getinverse<-function()inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse);
}

cacheSolve <- function(x, ...) {
  inv<- x$getinverse();
  if(!is.null(inv)){
      message("getting cached data");
      return(inv);
  }
  invdata <- x$get();
  ma<-matrix();
  if(nrow(invdata)>ncol(invdata))  {
    ma<-invdata[1:ncol(invdata),1:ncol(invdata)];
  }else{
    ma<-invdata[1:nrow(invdata),1:nrow(invdata)];
  }
  ## verifying if the matrix is invertible
  inv <- solve(ma,...);
  x$setinverse(inv);
  inv
  ## Return a matrix that is the inverse of 'x'
}
