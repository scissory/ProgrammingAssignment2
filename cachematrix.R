## Following the "makeVector" example given for this assignment,
## the functions below get matrices if they have been already computed 
## and create them if they don't exist
##
## Really functions change only a few things from the example:
## 1) arguement passed in and returned as a matrix vs. list
## 2) the operation to perform on data is "solve" vs "mean"
##
## Structurally, these functions do not need to be much different thant 
## the sample

## Test cases:  matrix of       7  2  1     equals      -2  8  -5
##                              0  3  -1                3  -11 7
##                             -3  4  -2                9  -34 21
##
## and  4 3     equals  -2  3
##      3 2     equals   3  2
        



## To call functions x[["operation", "set|get|setinverse|getinverse"]]()
makeCacheMatrix <- function(x = matrix()) {

        ## Pretty much everything from the exmple
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        
        
        # ## return matrix
        rm <- matrix(c(set, get, setinverse, getinverse), 
                     ncol=4,
                     dimnames = list("operation", c("set", "get", "setinverse", "getinverse")))

}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        ## Get matrix from "makeCachedMatrix" object and return cached value if exists
        i <- x[["operation", "getinverse"]]()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If not cached, get data, for extra credit 
        ## I put in check to see if matrix is invertible
        data <- x[["operation", "get"]]()
        if(det(data)==0) {
                message("Det of matrix = 0, inverse cannot be computed")
                return(i)
        }
        
        
        ## Solve, pass ... arguements along
        i <- solve(data, ...)
        
        ## Set inverse value and return
        x[["operation", "setinverse"]](i)
        
        # return matrix
        i
        
}
