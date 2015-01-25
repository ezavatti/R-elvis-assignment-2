## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object that cache its inverse.
## It returns a list of the functions defined in makeCacheMatrix as the 
## list(set = set, get = get, 
## setinver = setinver, getinver = getinver),
## which is the last statement.
## The inverse is assigned to the cache "getinver"


makeCacheMatrix <- function(x = matrix()) {
        inver<- NULL
        set<- function(y) {
                x<<-y
                inver<<-NULL
        }
        get<- function() x
        setinver<-function(solve) inver <<- solve
        getinver<- function() inver
        list(set=set, get=get,setinver=setinver, getinver=getinver)

}


## Write a short comment describing this function

## The function assesses if the cache inverse matrix is NULL (initial calculation) 
## or if the matrix, for which the inverse needs to be calculated, has changed 
## and therefore 
## a new cache needs to be calculated. 
## It does so by assessing IF the matrix is not NULL AND 
## if the inverse of the inverse matrix in the cache IS NOT equal (not identical)
## to the input matriz. 

##If both conditions are satisfied, then the matrix is different from 
## the one that originated the former cache,  
## the new cache inverse matrix is assigned 
## and the function will retrieve this last cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$getinver()
        if(!is.null(inver) & identical(solve(x), z) == FALSE){
                message("getting cached data")
                return(inver)
        }
        data<- x$get()
        inver<- solve(data,...)
        x$setinver(inver)
        inver
}
