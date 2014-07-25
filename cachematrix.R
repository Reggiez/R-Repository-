# The following is how to create a matrix object and calculate the inverse

makeCacheMatrix <- function(matrx = matrix()) {
        #initialization for Object
        matrixInv <- NULL
            #set-up step
			set <- function(y) {
                matrx <<- y
                matrixInv <<- NULL
        }
            #get step (matrix)
        get <- function() matrx
            #set-up Inverse step 
        setinvs <- function(inverse) matrixInv <<- inverse 
            #get Inverse cached 
        getinvs <- function() matrixInv 
            #get a list the available function 
        list(set = set, get = get,
            setinvs = setinvs,
            getinvs = getinvs)
}


#Get the inverse matrix. 
#If inverse has is calculated already, it will be retrieved

cacheSolve <- function(x, ...) {
            #Return matrix inverse of 'x' 
        invmatrx <- x$getinvs()
            #If the inverse exists, get it
            if(!is.null(invmatrx)) {
            #Return the inverse from cache
                message("getting cached data")
                return(invmatrx)
            }
            #Process if the cash is empty 
                data <- x$get()
            #Return the inverse of the matrix 
            invmatrx <- solve(data)
            #Setup-step to set the inverse 
            x$setinvs(invmatrx)
            #This will return the inverse
            invmatrx
}
