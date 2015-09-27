#makeCacheMatrix creates a special "matrix" object that can 
#cache its inverse.
#cacheSolve computes the inverse (or retrieves the cache) 
#of the special "matrix" returned by makeCacheMatrix. 


#Function:makeCacheMatrix
#Input:  A matrix
#Output: A special "matrix" that is really a list.
#Contains four functions that can be applied to 
#the output matrix:
# set() to set the value of the matrix;
# get() to get the value of the matrix;
# setInv() to set the value of the inverse matrix;
# getInv() to get the value of the inverse matrix.

makeCacheMatrix <- function(m = matrix())
{
     mInv <- NULL
     
     set <- function(x)
     {
          m <<- x
          mInv <<- NULL
     }
     
     get <- function()
     {
          m
     }
     
     setInv <- function(solve)
     {
          mInv <<- solve
     }
     
     getInv <- function()
     {
          mInv
     }
     
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


#Function:cacheSolve
#Input:  A special "matrix" created by makeCacheMatrix
#Output: The inverse matrix of the imput matrix.
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(m, ...)
{
     mInv <- m$getInv()
     
     if (!is.null(mInv))
     {
          message("Getting cached inverse")
          return(mInv)
     }
     
     matrixData <- m$get()
     inv <- solve(matrixData, ...)
     m$setInv(inv)
     inv
}