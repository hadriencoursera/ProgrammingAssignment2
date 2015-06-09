## Put comments here that give an overall description of what your
## functions do

## Create a list full of function needed to inverse a square matrix and cache the result in one environment
        ## set : able to reset the matrix and the inverse when new values
        ## get : return the matrix
        ## setinv : attrib a value to the inverse to be returned
        ## getinv : return the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## Verifie if the inverse of the matrix already exist in cache, else call the list of function returned 
## by "makeCacheMatrix" to solve the inverse and store the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("inverse already calculated in cache :")
                return(inv)
        }
        mat<- x$get()
        inv<- solve(mat,...)
        x$setinv(inv)
        inv
}



