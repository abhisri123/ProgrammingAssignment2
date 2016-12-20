## makeCacheMatrix cretaes a special matrix, which is a list 
##containing functions to get and set value for matrix,
## and get and set value for inverse
## It first sets up an inverse matrix with all values as NA
## and calls CacheSolve function to get the Inverse Matrix
## Otherwise existing inverse mtrix is retrieved 


makeCacheMatrix <- function(x = matrix()) {
  
      inv_x<-matrix(,nrow=dim(x)[1],ncol=dim(x)[2]
          
      set <- function(y) {
                  x <<- y
                  inv_x<<-matrix(,nrow=dim(x)[1],ncol=dim(x)[2])
                                 
       }
                
                
       get <- function() x
       setinv <- function(solve) inv_x <<- solve
       getinv <- function() inv_x
       
       list(set = set, 
            get = get,
            setinv = setinv,
            getinv = getinv)
}


## CacheSolve function to get cached inverse matrix if available in calling environment
## This is done by checking the first element of the inverse matrix if it is NA.
## and set computed values if not available in calling environment
## The computed Inverse Matrix is returned back to the calling function

cacheSolve <- function(x, ...) {
  
       inv_x<-x$getinv()
       if(!is.na(inv_x[1,1])){
       message("getting cached data")
       return(inv_x)
       }
  
       data <- x$get()
       inv_x <- solve(data)
       x$setinv(inv_x)
       inv_x
  
}

