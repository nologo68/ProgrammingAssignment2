## In this assignment 2, we want to write a pair of functions that cache the INVERSE OF A MATRIX.

## The following function (makeCacheMatrix) creates a list containing a function that creates 
#a special "matrix" object that can cache its inverse: 
  #set the content of the matrix.
  #get the content of the matrix.
  #set the inverse of the matrix.
  #get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #set the inverse to NULL as a placeholder for a future value.
        set <- function(y) { #defines a function to set the matrix, x, to a new matrix, y, and results the inverse.
                x <<- y #superoperators
                inv <<- NULL
        }
        get <- function() x #returns the matrix, x.
        setInverse <- function(inverse) inv <<- inverse #sets the inverse, inv, to inverse with the superoperator.
        getInverse <- function() inv #returns the inverse, inv.
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) #returns the list containing all the functions just defined.
}


#The following function (cacheSolve) evaluates the inverse of the special "matrix" returned by the function above. Moreover, if there is 
#inverse already calculated within the makeCacheMatrix function (and the matrix has not changed), it will take it as valid. Otherwise, this
#function will calculate the inverse of the matrix, set the inverse in the above function and retrieve it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() #search for the matrix inverse within the above function.
        if (!is.null(inv)) { #if the inverse exists... 
                message("getting cached data")
                return(inv) # ...it gets it!
        } #if the inverse is not within the function (or the matrix has changed)...
        mat <- x$get() #mat gets the matrix stored within the above function.
        inv <- solve(mat, ...) #inv calculates the inverse
        x$setInverse(inv) #stores the inverse in the object inv within the above function.
        inv #retrieves finally that inverse.
}
