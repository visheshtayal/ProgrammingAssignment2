## Pair of functions that cache the inverse of a matrix

## Following function creates a matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
   inverse<-NULL
   
   set<-function(matrix)
   {
     m <<-matrix
     inverse <<-NULL
   }
   
   get<-function()
   {
     m
   }
   
   setInverse<-function(inverse_val)
   {
     inverse<<-inverse_val
   }
   
   getInverse<-function()
   {
     inverse
   }
   
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function calculates the inverse of the matrix object given by the above function
## and if the matrix is unchanged then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        
   m<-x$getInverse()
   
   if(!is.null(m))
   {
     message("getting cached data")
     return (m)
   }
   
   data<-x$get()
   m<-solve(data)%*%data
   x$setInverse(m)
   
   m
}
