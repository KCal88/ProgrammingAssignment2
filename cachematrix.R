## The two functions here work to create a special list that contains 4
## functions related to performing tasks in such a way as to avoid expensive
## computations repeatedly (for example - matrix invesion as seen here). The
## first function (makeCacheMatrix) creates a special list that contains 
## operations to store and retrieve the data and the solution, while the 
## second function either retrieves the solution if it already exist, or
## performs the needed calculations to generate the solution and then store it.

## The first function makeCacheMatrix takes in a matrix that is invertable 
## (assumed input) and returns a list a four functions that perform the 
## following (as indexed)
##      1. get the value of the matrix passed into the function
##      2. set the value of the matrix and remove the old inversion solution
##         if applicable
##      3. set and store the value of the inverse (computed elesewhere)
##      4. get and report the inverted matrix if appilcable

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        print(x)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(sln) inv <<- sln
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## Perform the solution and store it in the "cache" of the special list vector.
## Will only do the expensive calculation if it doesn't already have an answer

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it already exists
        inv <- x$getinv()
        if(!is.null(inv)) {x
                message("getting cached inverse")
                return(inv)
        }
        ## Calcualte and store inverse of 'x' if it doesn't already exist
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Testing code

# set seed and do a test run
set.seed(41)
X <- matrix(rnorm(9),3,3)
M <- makeCacheMatrix(X)
cacheSolve(M)

# running a second time should yield "getting cached inverse" message
cacheSolve(M)


