#create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #create a matrix
inv <- NULL #create empty variable that will hold value of matrix inverse
set <- function(y) { 
  x <<- y #set x to the value you input
  inv <<- NULL
}
get <- function() x #define the get function to return the value of the matrix
setinverse <- function(inverse) inv <<- inverse #set the inverse value
getinverse <- function() inv #gets the value of the inverse
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns the value
}

#compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { #if the inverse is not null
    message("getting cached data") #indicate R is getting cached data
    return(inv) #return the matrix inverse
  } #if the inverse is null then do the below
  data <- x$get() #get the matrix data
  inv <- solve(data, ...) #create the inverse of the matrix through the solve function
  x$setinverse(inv) #set the inverse of the matrix
  inv #return the inverse of the matrix
}
