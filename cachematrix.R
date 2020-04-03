##These functions will first generate a matrix and then cache its inverse using the solve() function.

## With Regard to makeCacheMatrix() :
## x is an empty numeric vector, length 4 by default.
## Althougth it has a value of NULL
 ## m is an object that can be acted on later  
  ##with the '<<-' operator

makeCacheMatrix <- function(x=numeric(length= 4L)) { 
  
  
  m <- NULL                                        
  
  
  
  
  
  set <- function(y) {
    
    x <<- y
    m <<- NULL
  }
  ## the <<- form of the assignment operator assigns the value on the right to an object 
  ##in the parent enviroement. Since set() is a function within a function, the parent enviroment
  ## is the makeCacheMatrix() function. This has the effect of making x=y in the level above. 
  
  ##set() therefore assigns the value y to the x arguement of makeCacheMatrix
  ## and assigns the value NULL to m in the parent enviroment. This seems redundant since the latter
  ##assigment has already been made in the level above. However, it clears any value of m that had been
  ##previously cached by prior execution of chache mean(below), which also assigns a value of m.
  
  ##Whenever x is reset, therefore, the stored value of m chached in memory is cleared. 
  ##Cachemean must therefore be called again.
  
  ##Notice that set() is very similar to makeCacheMatrix: set value x, make m NULL. This concludes the set function.
  
  
  
  
  get <- function() x
  
  ##Since the symbol x is not defined within get(), R retrieves it from the parent enviroment- makeCacheMatrix().
  ##The line 'return(x)' is not required as 'x was the last thing evaluated in get', though it could be included here.
  
  
  setinverse <- function(inverse) m <<- inverse
  
  
  ##Note again that the reverse assignment operator assigns a different value to m, inverse.
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  ## This assigns each of these functions as a component of a list. Naming them in this way allows for use of the $ function
  ## rather than the [[]] function.
  
}

cachematrix <- function(x, ...) {
  m <- x$getinverse() ##get value of inverse and designate m
  if(!is.null(m)) { ##If this value is not null return the below.
    
    message("getting cached data")
    return(m)
  }
  
  ##If the value is Null the following code below will run                                       
  
  z <- x$get()                    #One could argue this line is not entirely required but it helped to 
  ##keep me orientated when editing from the original 'mean' function.
  data <- matrix(z, ncol = 2)
  
  ##This function creates a matrix object
  ## The matrix object created has two columns
  ## It is a numeric vector with default
  ## length 4. If the input is not a multiple
  ## or sub-multiple of the number of rows
  ## input is recycled and a warning message is
  ## displayed.
  
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


########################### Acknowledgements : ##############################
##I found this assignment very difficult, given that my knowledge of matricies is poor
##and that it required knowledge outside of what was covered in the course.
##I have relied heavily on the following help pages, which my explanations will likley resemble closely:

## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/mowodw4rEemS0Q4U5mc4kg

## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


