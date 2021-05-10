makeCacheMatrix<-function (x=matrix()){
  inv<-NULL
  set<-function(y){
    
    inv<<-NULL
    x<<-y
    
  }
  get<-function(){x}
  setInv<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set, get=get, setInv=setInv, getInverse=getInverse)
}

cacheSolve<-function(z, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("get cache data")
    return(inv)
  }
  mat<-x$get()
  
  x$setInv(inv)
  
  inv<-solve(mat, ...)
  
  inv
  
}