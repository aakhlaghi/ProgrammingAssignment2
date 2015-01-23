## The overall aim of the two functions in this file is to implement caching mechanism 
## for previously calculated inverse of a matrix. There are two function in this file.  
## The first function gets a matrix and tries to make another object which actually is a 
## list and the list has four elements that are functions for getting and setting the same 
## passed  matrix and also functions for getting and setting solve which is supposed to be the 
## inverse of the passed matrix. The second function gets the object or the list created 
## from first function and tries to compute inverse of the matrix if it was not already 
## generated or returns the already generated one to save computation time and resources.


## This function gets a matrix as its argument and creates and returns a list of 
## four functions. The first two functions being used to get and set the passed 
## matrix and the other two functions are used to get and set the solve which is 
## supposed to be the invesre of the passed matrix to the function.

makeCacheMatrix <- function(x = matrix()) {
  
  solve <- NULL
  
  getMatrix <- function() x;
  
  setMatrix <- function(m) {
    
    solve <<- NULL
    x <<- m
  }
  
  getSolve <- function() solve
  
  setSolve <- function(s) solve <<- s 
  
  list(getMatrix = getMatrix, setMatrix = setMatrix,
       getSolve = getSolve, setSolve = setSolve)
}


## This function gets a list of the four functions generated from makeCacheMatrix 
## function and generates the solve which is the inverse of matrix in the passed argument. 
## In case the inverse was already calculated and set it returns the already calculated 
## one. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  solve <- x$getSolve()
  
  if (!is.null(solve)) {
    
    message("Invesre of matrix already calculated!")
    return(solve)
  }
  
  solve <- solve(x$getMatrix())
  
  x$setSolve(solve)
  
  solve
}
