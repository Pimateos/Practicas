makeCacheMatrix <- function(x = matrix()) {
 
  #The first function, makeVector creates a special "vector", which is really a list containing a function to
  #1.set the value of the matrix
  #2.get the value of the matrix
  #3.set the value of the inverse
  #4.get the value of the inverse
  
  m = NULL
  set = function(y) 
    {
      x <<- y
      m <<- NULL
    }
  get = function() x
  setinverse = function(inverse) m <<- inverse 
  getinverse = function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  #Look if the inverse is already calculated
  m = x$getinverse()
  if (!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
  
 #If you get here, the inverse have not already calculated. Therefore, you have to calculate
  matriz = x$get()
  m = solve(matriz)
  x$setinverse(m)
  return(m)
}
test = function(matriz){
  
      temp = makeCacheMatrix(matriz)
      #This time calculate the inverse
      cacheSolve(temp)
      #This time get it from the cache
      cacheSolve(temp)
}

matriz<-matrix(1:4,nrow=2, ncol=2)
test(matriz)