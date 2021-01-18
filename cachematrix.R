## Put comments here that give an overall description of what your
## functions do

## This is one of the activity for Coursera, using R Programming. 
## This program is a function that will enable to inverse the matrix.
## Using cache matrix in the R Programming 
## January 18, 2021, Assignment for Week #3, JJLM


makeCacheMatrix <- function(M = matrix()) {
    inv <- NULL                             ## The NULL is the one that will perform using the inv function. This will hold the value of the inverse matrix
    set <- function(J) {                    ## use the function J for a new assign
        M <<- J                             ## Use the variable M and J
        inv <<- NULL                        ## If a new value of matrix will created, reset inv to NULL
    }
    get <- function() M                     ## get the value of function that will define the value of M and then get the value of the matrix

    setinverse <- function(inverse) inv <<- inverse  ## Use the value of M and J to create the new matrix
    getinverse <- function() inv                     ## Use this function to determine the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## Use this to develop the function
                                                         
}


## Write a short comment describing this function
## This function will enable to define the value of the inverse matrix by using the file from github
## If the program will not able to calculate the matrix, use the cacheSolve file to solve the inverse matrix

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- M$getinverse()			   ## Use the value of M
    if(!is.null(inv)) {				   ## Null into the inv
        message("getting cached data")	   ## Use the cachedata for cachematrix
        return(inv)
    }
    data <- M$get()				   ## use the M get function
    inv <- solve(data, ...)			   ## Use the Solve function
    M$setinverse(inv)				   ## Inverse the matrix 
    inv
}
