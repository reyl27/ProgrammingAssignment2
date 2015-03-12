## The following two functions are used for working with matrices.
##
## makeCacheMatrix: This function creates a matrix object that holds 
## the matrix data, its inverse if calculated, and any associated
## error messages that occurred during the inverse calculation.
##
## cacheSolve: This function solves for the inverse of a matrix 
## set into the matrix object that's created using makeCacheMatrix.

## <Description>
## Use this function to create a cached matrix object. The object
## will contain the matrix and a placeholder (property) for the
## matrix inverse.
## E.g. cMat <- makeCacheMatrix(matrix(c(1,2,1,2), nrow = 2, ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
        # Properties of matrix
        invx <- NULL # Matrix inverse
        invMsg <- '' # Error message for the inverse calculations
        
        # Initialize matrix with y, resets all cached properties:
        set <- function (y) {
                x <<- y
                invx <<- NULL
                invMsg <<- ''
        }
        
        # Retrieve the matrix:
        get <- function() x
        
        # Set the matrix inverse:
        setInv <- function(inverse) invx <<- inverse
        
        # Get the matrix inverse:
        getInv <- function() invx
        
        # Set the error message:
        setMsg <- function(msg) invMsg <<- msg
        
        # Get the error message:
        getMsg <- function() invMsg
        
        # List of accessors:
        list(set = set, get = get, 
             setInv = setInv, 
             getInv = getInv,
             setMsg = setMsg,
             getMsg = getMsg)
}

## <Description>
## Use this to solve for the inverse of a matrix stored inside
## a cached matrix object (x). The answer will either be a
## matrix or NaN depending on the validity of the matrix.
## E.g. cacheSolve(cMat)
cacheSolve <- function(x, ...) {
        # Check if input x is a cache matrix object, return NULL and
        # message if not:
        if (!exists('getInv', envir = x)) {
                message('Invalid input.')
                return(NULL)
        }
        
        # Check if the inverse is already in the cached matrix,
        # return cached data if it exists:
        invx <- x$getInv()
        if (!is.null(invx)) {
                  # Check if NaN, if it is, there should be an error
                  # message to be displayed as well in the cached data:
                  if (length(invx) == 1 && is.na(invx)) { message(x$getMsg())}
                  return(invx)
        }
        
        # Inverse isn't cached, retrieve the matrix from cached
        # matrix object:
        matrix <- x$get()
        
        # Solve for the inverse using 'solve', the assumption is
        # that the matrix is square and invertible, results are
        # meaningless and unobtainable otherwise. Therefore size
        # and determinant tests are implemented here.
        
        # Note: Arbitrarily decided that 'NaN' will indicate that
        # no inverse can be found.
        
        # Determine if matrix is square by looking at its
        # dimensions:
        matSize <- dim(matrix)
        if (length(matSize) != 2 || matSize[1] != matSize[2]) {
                invMsg <- 'Non-square matrix.'
                message(invMsg)
                x$setMsg(invMsg)
                invx <- NaN
        }
        else {  
                # Square matrix invertible iff determinant of the
                # matrix is non-zero:
                if (det(matrix) == 0) {
                        invMsg <- 'Non-singular matrix.'
                        message(invMsg)
                        x$setMsg(invMsg)
                        invx <- NaN
                }
                else {
                        invx <- solve(matrix)
                }
        }
        
        # Set the inverse in the cached matrix variable:
        x$setInv(invx)
        
        # 'return' the inverse:
        invx
}
