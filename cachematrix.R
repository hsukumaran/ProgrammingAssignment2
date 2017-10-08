##Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## Fist Function makeCacheMatrix() is to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
## step1: checks if the the argument passed if of type matrix. 
        if(!is.matrix(x)) stop("x must be a matrix")
## step2: set the value of the matrix
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
## step3: get the value of matrix
        get <- function() x

## step 4: set the value of the inverse of the matrix
        setinverse <- function(inv) minv <<- inv
        getinverse <- function() minv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This functions computes the inverse of matrix if inverse has not been computed already. 
#If inverse was already computed it get the results from cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
#set inverse matrix to minv, so when the cacheSolve function is called again it passes 
#!is.null(minv) condition and skips the computation part and returns the value stored in minv.
        x$setinverse(minv)
        minv
}

##Test Run:
##> mat<-matrix(1:4,2,2)
##> fmat<-makeCacheMatrix(mat)
##> fmat$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## > fmat$getinverse()
## NULL
## Firt run :-
## No cache in first run hence inverse is computed
## > cacheSolve(fmat)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## Second Run:-
## Retrieving from cache in second run
## > cacheSolve(fmat)
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


