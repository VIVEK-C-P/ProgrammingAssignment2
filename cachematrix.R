## Cache the inverse of a matrix
## Function makeCacheMatrix creates a datastructure 
## which stores the inverse of a matrix passed
## Function cacheSolve utlizes the matrix created by makeCacheMatrix 
## to actually store the inverse of the matrix if it has not been done by the function.

## This function creates a data structure which stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)i<<-inverse
        getinverse<-function()i
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function stores the inverse of the matrix if it is not present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                print("getting the cached version of inverse")
                return(i)
        }
        mat<-x$get()
        i<-solve(mat)
        x$setinverse(i)
        i
}
