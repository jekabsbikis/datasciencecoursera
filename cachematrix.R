#This is an asignment for JHU "Programming with R" course in Coursera
#submitted in March 2020.
#Big thanks to those peer-reviewing, and good luck on your own studies!

#The functions below take a square matrix and return an inverse of the given 
#matrix, first checking whether there is a solution (an inverse) for this matrix
#that has already been stored in cache (and if so - returning this solution) and,
#if there is not a solution in the cache - calculating and returning the solution anew.


#makeCacheMatrix() is the first of two main-level functions in cachematrix.R
#I define the function and initialize a variable x which will take a matrix form.
#then I initialize variable s (I used 's' for 'solved matrix) which I set to NULL
#next I create first of 4 second-level functions
#  set() assigns input argument to the x object in the parent environment... 
#        and sets s to NULL to clear any value of s that had been stored in cache
#        before the program recalculates the inverse for a new matrix. 
#  get() pulls x from the parent environment
#  setSolution() takes the input argument and passes it on to s in the parent env.
#  getSolution() retreives the value of s
#In the list I assign each of the above 4 functions as elements in a list to be 
#returned to the parent environment. The reason for doing this is that giving 
#list elements names will allow me to invoke them later with the '$' sign,
#such as by x$getSolution, x$get, or x$setSolution
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
          x <<- y
      s <<- NULL
      }
  get <- function () x
  setSolution <- function(solution) s <<- solution
  getSolution <- function() s
  list(set = set, 
       get = get, 
       setSolution = setSolution, 
       getSolution = getSolution)
}

#cacheSolve() is the second main-level function. Its purpose is to populate
#the solution (the inverse of the given matrix) and/or to retrieve the inverse
#from MakeCacheInverse(). The function attempts to get 'the solution' (an inverse
# of a given matrix), checks whether there is a solution in cache (and if there is, 
# it prints a message along with the existing solution), gets the matrix from 
#the input objet, calculates its inverse, caches the solution as the inverse, and passes
#back the solution.
cacheSolve <- function(x, ...) {
      s <- x$getSolution()
  if (!is.null(s)) {
      message("getting cached data")
      return(s)
  }
  matrix_data <- x$get()
  s <- solve(matrix_data, ...)
  x$setSolution(s)
  s
}

