##  makeCacheMatrix() and cacheSolve() work together to first store a matrix within makeCacheMatrix() and
##  then calculate the inverse in cacheSolve() while also storing the inverse in cache in makeCacheMatrix().
##  cacheSolve() checks the cache prior to calculating the inverse of the the supplied matrix and only performs 
##  the calculation if no inverse is already cached for the supplied matrix.

## makeCacheMatrix(x, ...), Argument: x = invertible matrix
## makeCacheMatrix() stores a matrix as "x", provides "x", stores the inverse of "x" as provided by 
## cacheSolve(), and provides or prints the inverse as requested by cacheSolve().
## 
## 
## Hint: Set results of makeCacheMatrix as an object in R then call results as object$get() for stored matrix, 
## or object$getinverse() for stored matrix inverse.
## For proper function do not call object$setinverse().


makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        
        
        set <- function(y){
                x <<- y
                m <<- NULL  
        }
        
        
        get<-function() {
                x
        }  
        
       
        setinverse<- function(solve) {
                m<<-solve
        }
        
            
        getinverse <-function(){
                m
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x,...) Argument: "x" supplied as an object containing the results of makeCacheMatrix()
## cacheSolve() checks the makeCacheMatrix() to determine if the inverse of the supplied matrix has already 
## been calculated and if so, provides the cached inverse from makeCacheMatrix(), and if not, calculates the inverse of the supplied 
## matrix and stores the result in cache in makeCacheMatrix().

cacheSolve <-function(x,...){
        m<-x$getinverse()
        
              
        if(!is.null(m)){
                message("getting cached data")
                return(m)        
        }
        
        data<-x$get()

        m<-solve(data,...)
        x$setinverse(m)
        m
       
}
