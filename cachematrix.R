# I have defined two functions, one with the target set and get both the matrix
# on which devel obtain its inverse, as another function that obtains the 
# inverse matrix by the last parameter.
# The logical order of execution is:
# 1 Define and establish the matrix on which you want to obtain the inverse
# 2 Get the inverse matrix.
# 
# La segunda función comprueba si la inversa ya esta calculada, en ese caso, 
# se devuelve, si no fuera así, se calcula la inversa y se devuelve. 
# 
# Example:        
# > source ("/ Users/<username>/ProgrammingAssignment2/cachematrix.R")
# > A <- makeCacheMatrix (matrix (1:4,2,2))
# > A$get ()
#     [,1] [,2]
# [1]    1    3
# [2]    2    4
# > B <-cacheSolve (A)
# > B
#     [,1] [,2]
# [1]   -2    1.5
# [2]    1   -0.5
# > B <-cacheSolve (A)
# getting cached data
# > B
#     [,1] [,2]
# [1]   -2    1.5
# [2]    1   -0.5
#########################################
#
# Function that allows you to set the matrix on which 
# devel calculate its inverse, also obtain this matrix and its inverse
#
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)    
}
#
# Function that calculates the inverse of a matrix if reviewing 
# previously calculated
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
}