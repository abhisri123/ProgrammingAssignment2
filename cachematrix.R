## makeCacheMatrix cretaes a special matrix, which is a list 
##containing function to set and get value of matrix,
##get and set value of inverse
## It sets up an inverse matrix with all values as NA
##If the first element is NA then then the value is set
## Otherwise existing inverse mtrix is retrieved 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x<-matrix(,nrow=dim(x)[1],ncol=dim(x)[2]
                
                set <- function(y) {
                  x <<- y
                  inv_x<<-matrix(,nrow=dim(x)[1],ncol=dim(x)[2]
                                 
                }
                
                
                get <- function() x
                setinv <- function(solve) inv_x <<- solve
                getinv <- function() inv_x
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}




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

