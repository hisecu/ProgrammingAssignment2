##
## rprog-011 Programming Assignment 2,
##
## Course URI:
##   https://class.coursera.org/rprog-011/
##
## Assignment URI:
##   https://class.coursera.org/rprog-011/human_grading/view/courses/973492/assessments/3/submissions
##
## Note that we strive for a relatively simple approach.
## If necessary, it can be made more sophisticated
## at a later point.
##
## Carol Fenijn, February 2015.
#

##
## makeCacheMatrix is a function that will create the inverse matrix object
## of a given input matrix object. 
##
## This inverse matrix object will be cached, so that it can be retrieved
## efficiently without the need of computing it over and over again, which
## would be very costly. 
##
## For this to work, we set values of variables in the more global
## parent name space / environment in functions. Normally, we would try
## to avoid techniques like these, but we are asked to do this as part
## of this assignment. 
##

makeCacheMatrix <- function(x = matrix()) {

        #
        # Initialize inversematrix
        #
        inversematrix <- NULL

        #
        # Define the set() function. 
        #
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }

        #
        # Define the get() function. 
        #
        get <- function() {
                x
        }

        #
        # Define the get() function. 
        #
        setinversematrix <- function(inversematrix) {
                # Note that inversematrix is set more globally
                # here, so in the parent name space / environment
                inversematrix <<- cacheSolve(x)
        }


        #
        # Define the get() function. 
        #
        getinversematrix <- function() {
                inversematrix
        }

        #
        # The following list of functions is returned, so the functions
        # are accessible as makeCacheMatrix$get(), makeCacheMatrix$invertmatrix() etc.
        #
        list(
             set = set,
             get = get,
             invertmatrix = invertmatrix,
             getinversematrix = getinversematrix
            )
}


##
## cacheSolve is a function that will return the inverse matrix
## of a given input matrix.
##
## If the inverse matrix has been computed before, a cached
## version of it will be returned.
##
## If the inverse matrix has not been computed before, it will
## be computed on the fly by cacheSolve() and it will be added
## to the cache.
##

cacheSolve <- function(x, ...) {
    ##
    ## Return a matrix that is the inverse of 'x'
    ##
    #
    # I find input_m more readable than x, as it is the input matrix:
    #
    retrieved_m <- x$getinversematrix()

    #
    # calculate the inverse of input_m:
    #
    if(is.null(retrieved_m)) {
            inverse_m <- solve(retrieved_m)
    }
    else
    {
            message("getting cached data")
            inverse_m <- retrieved_m
    }
      
    #
    # Return the inverse matrix of input_m:
    #
    return(inverse_m)
}

#
#sample_m <- rbind(c(00000001, 00000002), c(00000003, 00000004))
#
#class(sample_m)
#
#sample_m
#
#inv_sample_m <- cacheSolve(sample_m)
#
#inv_sample_m
#
#sample2_m <- matrix( c(1, 1, 3, 1), nrow=2, ncol=2)
#
#class(sample2_m)
#
#inv_sample2_m <- cacheSolve(sample2_m)
#
#inv_sample2_m
#
#
