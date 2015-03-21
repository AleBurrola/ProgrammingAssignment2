## caching the inverse of a matrix using 2 functions:
## makeCacheMatrix and cacheSolve


## creates a list to set and get the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()){
  
  ## initializes solve variable as null
  s <- NULL
  
  ## assigns values of x and s at the makeCacheMatrix environment
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  
  ## assigns the matrix
  get <- function() x
  
  ## takes the value of solve and assigns it to s
  setsolve <- function(solve) s <<- solve
  
  ## assigns the value of s
  getsolve <- function() s
  
  ## delivers the lists of the previously defined functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculates the inverse of a matrix
## previously checks if the inverse has already been calculated

cacheSolve <- function(x=matrix(),...){
  
  # assigns the value of getsolve (defined at the makeCacheMatrix function)
  s <- x$getsolve()
  
  # checks if the value is different than null
  if(!is.null(s)){
    
    ## if it is different than null, it shows a message and returns its value
    message("getting cached data")
    return(s)
  }
  
  #assigns the value of get (defined at the makeCacheMatrix function)
  data <- x$get()
  
  #calculates the inverse of the matrix
  s <- solve(data,...)
  
  # assigns the value of the inverse
  x$setsolve(s)
  
  # displays the inverse of the matrix
  s
}
