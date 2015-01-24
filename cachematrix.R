## These functions are used for finding and caching the inverse of a matrix. If the inverse has
## been calculated previously the cached inverse will be returned otherwise the inverse will be 
## calculated and returned. To use these functions first pass the matrix to the makeCacheMatrix
## function. The output of the makeCacheMatrix can then be passed to the cacheSolve function to
## calculate or retrieve the inverse of the matrix. To change which matrix the inverse is being 
## computed for use x$set_working_matrix where x is the output of the makeCacheMatrix.


## This function caches a matrix and provides a vector of functions for setting and retreiving
## the working matrix and the inverse of the matrix

makeCacheMatrix <- function(mx = matrix()) {
  inv<-NULL
  
  set_working_matrix <-function(y){
    mx<<-y
    inv<<-null
  }
  get_working_matrix <- function() mx
  set_inv <- function(invToCache) inv<<-invToCache 
  get_inv <- function() inv
  
  list(set_working_matrix = set_working_matrix,
       get_working_matrix = get_working_matrix,
       set_inv = set_inv,
       get_inv = get_inv)
}



## This function takes a vector of function and uses it to recall the cached inverse of a matrix
## set by makeCacheMatrix or to compute it if it has not been previously calculated.

cacheSolve <- function(x, ...) {
  inv<-x$get_inv()
  if(!is.null(inv)){
    message('Retreiving cached inverse')
    return(inv)
  }
  mx<-x$get_working_matrix()
  inv<-solve(mx, ...)
  x$set_inv(inv)
  inv
}
