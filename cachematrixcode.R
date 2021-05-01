cachematrix <- function(m=matrix()){
  k <- NULL
  setmat<-function(matrix){
    m<<-matrix
    k<<-NULL
  }
  get<-function(){
    m
  }
  setmatinverse<-function(inverse){
    k<<-inverse
  }
  getinverse<-function(){
    k
  }
  list(setmat=setmat, get=get, setmatinverse=setmatinverse, getinverse=getinverse)
}
solvecache<-function(x,...){
  m<-x$getinverse()
  if( !is.null(m)){
    message("get cache data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)%*% data
  x$setmatinverse(m)
  m
}