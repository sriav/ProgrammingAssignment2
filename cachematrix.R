## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#1. makeCacheMatrix basically is an index of the input data that is sent in for 
#evaulation. This can store and retrieve the matrix and the inversere of the matrix
#2. This is done @ 4 levels (set & get) sets (caches) the values & returns the value of the matrix
#& similarly is the case with setinverse & get inverse which caches & retrieves the values. This takes the input from the 


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

  }

## Write a short comment describing this function
#1. This is the actual function that caculates & displays the inverse of the matrix that 
#is returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
#Check if the inverse values are present in the makeCacheMatrix before computing if yes, print it
    message("getting the cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
  }


# Sample solution

# x = rbind(c(1, -2/4), c(-3/4, 1))
# 
# c <- makeCacheMatrix(z)
# ans =cacheSolve(c)
# x
# ans
# 
##Final Answer
# > x
# [,1] [,2]
# [1,]  1.00 -0.5
# [2,] -0.75  1.0
# > ans
# [,1] [,2]
# [1,]  1.6  0.8
# [2,]  1.2  1.6
