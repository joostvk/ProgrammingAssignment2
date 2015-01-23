##	Function makeCacheMatrix returns a list of functions 
##  Creates empty object m, in this object the inversed matrix will be saved
##  Function set_inv_matrix assigns computed inverse matrix (of x) to m
##  Function get() obtains input matrix 
##  Function get_inv_matrixto obtains the cached inverse matrix m

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set_inv_matrix <- function(inv_matrix) m <<- inv_matrix
    get <- function() x
    get_inv_matrix <- function() m
    # return a list of functions as an R object
    list(get=get, set_inv_matrix=set_inv_matrix, get_inv_matrix=get_inv_matrix)
}

##	Function cacheSolve inverses the matrix. First it checks whether the invers 
##  of the input matrix has already been calculated.
##  If this is the case the already calculated inversed matrix is returned.
##	If not the matrix is inversed and returned.

cacheSolve <- function(x) {
    m <- x$get_inv_matrix()
    if(!is.null(m)){
        message("Inverse already calculated, getting cached data.")
        return(m)
    }
    else {
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$set_inv_matrix(m) # assigns resulting inverse matrix to object x
        return(m)
    }
}
