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

##
## makeCacheMatrix is a function that will make various other functions available.
##
## An inverse matrix object will be cached with the setmatrix() function, so that
## it can be retrieved efficiently without the need of computing it over and over
## again, which would be very costly. 
##
## For this to work, we set values of variables in the more global
## parent name space / environment in functions.
##
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL    # initialization of m

        #
        # Define the set() function. This will set a value to a variable in 
        # the more global name space / environment
        #
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        #
        # Define the get() function. It will just return a value 
        #
        get <- function() x

        # Note that inversematrix is set more globally
        # here, so in the parent name space / environment
        setmatrix <- function(inv_matrix) m <<- inv_matrix

        getmatrix <- function() m

        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
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
        m <- x$getmatrix()
        if(is.null(m)) {
                data <- x$get()
                m <- solve(data, ...)
                x$setmatrix(m)
        }
        else
        {
                message("getting cached data")
        }
        return(m)
}

