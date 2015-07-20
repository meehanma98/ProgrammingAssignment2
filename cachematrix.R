## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - this basically follows the same code as 
## the example in programming assignment 3 with changes from "mean" to "inv

makeCacheMatrix <- function(x = matrix()) {
        m = NULL # make sure the local matrix is empty before assigning the calling matrix to it
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x # call the annonymous function - ginv in this case
        setinv <- function(my_inv) m <<- my_inv #perform the inverse calculations
        getinv <- function() m #used to get the matrix, if NULL it has not been calculated
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## Write a short comment describing this function
## Again, this pretty much follows the example from the programming assignment. I decided
## to use the ginv function from the MASS package so there is a 'require' statement at first
## to load this package if not already done so. I did not put any 'tests' around this to make 
## sure the package was available but that would be a good idea for production code.

cacheSolve <- function(x, ...) {
        require(MASS) # I want to use the ginv function to solve this 
        m <- x$getinv() # set the variable to the current value of the cached matrix - NULL if not, of course
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # return the previously calculated inverse matrix
        }
        data <- x$get() # if the matrix has not been calcuated previously, begin to do so now
        m <- ginv(data,...) #use the ginv function from MASS to calculate the inverse
        x$setinv(m) #set the higher level matrix to the calculated values so you don't do it again
        m
        ## Return a matrix that is the inverse of 'x'
}
