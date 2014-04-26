## The 2 functions below helps to get the inverse of a Matrix and to cache it.

## This function receives a function as a parameter and stores it so you can get the values of the matrix that you introduce as a parameter.
## Also computes the inverse of the matrix that is passed as a parameter and keep it in a variable to avoid recompute it again.

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
      set<-function(y){
        x<<-y
        inverse<<-NULL
      }
    get<-function(){
      x
    }
    setinverse<-function(inv){
      inverse<<-inv
    }
    getinverse<-function(){
      inverse
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function receives a the list produced by the function above as a paremeter, and using the functions described above, if
## already had computed the inverse of the matrix, it returns the cached value, if not, it computes it and returns the inverse matrix of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
